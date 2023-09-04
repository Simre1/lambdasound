module LambdaSound.Sound.MSC where

import Control.Monad
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Trans.Class (MonadTrans (..))
import Control.Monad.Trans.Reader
import Control.Monad.Trans.State.Strict
import Control.Monad.Trans.Writer.CPS
import Data.Bifunctor (Bifunctor (first))
import Data.HashTable.IO qualified as H
import Data.Massiv.Array qualified as M
import Data.Maybe (fromJust)
import Data.Monoid
import Data.SomeStableName (makeSomeStableName)
import Data.Vector.Storable qualified as V (unsafeFromForeignPtr0)
import Data.Vector.Storable.Mutable qualified as VM (unsafeFromForeignPtr0)
import Foreign.ForeignPtr (newForeignPtr_)
import Foreign.Marshal (copyBytes, free, mallocBytes)
import Foreign.Ptr
import Foreign.Storable (Storable (..))
import LambdaSound.Sound.ComputeSound
import LambdaSound.Sound.Types

makeIndexCompute :: (SampleRate -> Int -> a) -> MSC (ComputeSound a)
makeIndexCompute f = do
  sr <- getSR
  stableF <- makeSomeStableName f
  pure $
    ComputeSound
      ( \chooseIc _ -> chooseIc $ const $ pure $ \index ->
          pure $ f sr index
      )
      (ComputationInfoMakeIndexCompute stableF)
{-# INLINE makeIndexCompute #-}

modifyIndexCompute :: (SampleRate -> SampleRate) -> (SampleRate -> (Int -> IO a) -> Int -> IO b) -> MSC (ComputeSound a) -> MSC (ComputeSound b)
modifyIndexCompute changeSr compute msc = do
  sr <- getSR
  stableChangeSr <- makeSomeStableName changeSr
  stableCompute <- makeSomeStableName compute
  (ic, subCi) <- withSR (changeSr sr) $ asIndexCompute msc
  pure $
    ComputeSound
      ( \chooseIc _ -> chooseIc $ \basePtr -> do
          oldCompute <- ic basePtr
          pure $ compute sr oldCompute
      )
      (ComputationInfoModifyIndexCompute stableChangeSr stableCompute subCi)
{-# INLINE modifyIndexCompute #-}

computeSequentially :: Float -> MSC (ComputeSound Pulse) -> MSC (ComputeSound Pulse) -> MSC (ComputeSound Pulse)
computeSequentially factor c1 c2 =
  do
    sr <- getSR
    let splitIndex =
          round $
            factor * fromIntegral sr.samples
    (w1, p1) <- withSR (sr {samples = splitIndex}) $ asWriteMemory c1
    (w2, p2) <- withSR (sr {samples = sr.samples - splitIndex}) $ asWriteMemory c2
    pure $
      ComputeSound
        ( \_ chooseWm -> chooseWm $ \basePtr ptr ->
            w1 basePtr ptr >> w2 basePtr (ptr `plusPtr` (splitIndex * pulseSize))
        )
        (ComputationInfoSequentially p1 p2)
{-# INLINE computeSequentially #-}

computeParallel :: MSC (ComputeSound Pulse) -> Float -> MSC (ComputeSound Pulse) -> MSC (ComputeSound Pulse)
computeParallel c1 factor c2 = do
  sr <- getSR
  let c1N = sr.samples
      c2N = round $ factor * fromIntegral sr.samples
  (ic1, p1) <- withSR (sr {samples = c1N}) $ asIndexCompute c1
  (ic2, p2) <- withSR (sr {samples = c2N}) $ asIndexCompute c2
  pure $
    ComputeSound
      ( \chooseIc _ -> chooseIc $ \basePtr -> do
          ic1' <- ic1 basePtr
          ic2' <- ic2 basePtr
          pure $ \index ->
            (+)
              <$> ic1' index
              <*> if index < c2N then ic2' index else pure 0
      )
      (ComputationInfoParallel p1 p2)
{-# INLINE computeParallel #-}

mapComputeSound :: (a -> b) -> MSC (ComputeSound a) -> MSC (ComputeSound b)
mapComputeSound f cs = do
  (ic, p) <- first (fmap (fmap (fmap f .))) <$> asIndexCompute cs
  stableF <- makeSomeStableName f
  pure $ ComputeSound (\chooseIc _ -> chooseIc ic) (ComputationInfoMap stableF p)
{-# INLINE mapComputeSound #-}

mapWholeComputation :: (M.Load r M.Ix1 Pulse) => (M.Vector M.S Pulse -> M.Vector r Pulse) -> MSC (ComputeSound Pulse) -> MSC (ComputeSound Pulse)
mapWholeComputation f msc = do
  sr <- getSR
  ptrOffset <- arenaAllocate
  (wm, subCi) <- asWriteMemory msc
  stableF <- makeSomeStableName f
  pure $
    ComputeSound
      ( \_ chooseWm -> chooseWm $ \basePtr ptr -> do
          let wholeSoundPtr = ptrOffset basePtr
          wm basePtr wholeSoundPtr
          wholeSound <- vectorFromPtr wholeSoundPtr sr.samples
          convolved <- mVectorFromPtr ptr sr.samples
          M.computeInto convolved $ f wholeSound
      )
      (ComputationInfoMapWholeComputation stableF subCi)
{-# INLINE mapWholeComputation #-}

asIndexCompute :: MSC (ComputeSound a) -> MSC (Ptr Pulse -> IO (Int -> IO a), ComputationInfo)
asIndexCompute msc = do
  (ComputeSound f p) <- msc
  f
    (pure . (,p))
    ( \w -> do
        ptrOffset <- arenaAllocate
        pure
          ( \basePtr ->
              do
                let ptr = ptrOffset basePtr
                w basePtr ptr
                pure $ \index -> peek (ptr `plusPtr` (index * pulseSize)),
            p
          )
    )
{-# INLINE asIndexCompute #-}

asWriteMemory :: MSC (ComputeSound Pulse) -> MSC (Ptr Pulse -> Ptr Pulse -> IO (), ComputationInfo)
asWriteMemory mcs =
  mcs >>= \(ComputeSound f p) ->
    f
      ( \ic -> do
          sr <- MSC ask
          memoized <- lookupMemoizedComputeSound sr p
          case memoized of
            Just getPtr -> do
              pure
                ( \_basePtr ptr -> do
                    source <- getPtr
                    -- putStrLn ("From:" ++ show source ++ " To:" ++ show ptr)
                    copyBytes ptr source (sr.samples * pulseSize),
                  p
                )
            Nothing -> do
              writePtr <- memoizeComputeSound sr p
              pure
                ( \basePtr ptr -> do
                    writePtr ptr
                    when (sr.samples > 0) $ do
                      ic' <- ic basePtr
                      forM_ [0 .. pred sr.samples] $ \i -> do
                        v <- ic' i
                        poke (ptr `plusPtr` (i * pulseSize)) v,
                  p
                )
      )
      (pure . (,p))
{-# INLINE asWriteMemory #-}

pulseSize :: Int
pulseSize = sizeOf (undefined :: Pulse)

newtype MSC a = MSC (ReaderT SampleRate (WriterT (Sum Int) (StateT (Int, MemoComputeSound) IO)) a) deriving (Functor, Applicative, Monad, MonadIO)

newtype MemoComputeSound = MemoComputeSound (H.BasicHashTable (SampleRate, ComputationInfo) (Ptr Pulse))

runMSC :: SampleRate -> MSC (ComputeSound Pulse) -> Ptr Pulse -> IO ()
runMSC sr msc samplePtr = do
  let (MSC r) = asWriteMemory msc
  hashTable <- H.new
  ((writeSamples, _ci), Sum size) <- evalStateT (runWriterT (runReaderT r sr)) (0, MemoComputeSound hashTable)
  arenaPtr <- mallocBytes (size * sizeOf (undefined :: Pulse))
  writeSamples arenaPtr samplePtr
  free arenaPtr
{-# INLINE runMSC #-}

zipWithCompute :: (a -> b -> c) -> MSC (ComputeSound a) -> MSC (ComputeSound b) -> MSC (ComputeSound c)
zipWithCompute f msc1 msc2 = do
  (ic1, p1) <- asIndexCompute msc1
  (ic2, p2) <- asIndexCompute msc2
  stableF <- makeSomeStableName f
  pure $
    ComputeSound
      ( \chooseIc _ -> chooseIc $ \basePtr -> do
          ic1' <- ic1 basePtr
          ic2' <- ic2 basePtr
          pure $ \i -> f <$> ic1' i <*> ic2' i
      )
      (ComputationInfoZip stableF p1 p2)
{-# INLINE zipWithCompute #-}

getSR :: MSC SampleRate
getSR = MSC ask
{-# INLINE getSR #-}

withSR :: SampleRate -> MSC a -> MSC a
withSR sr (MSC r) = MSC $ local (const sr) r
{-# INLINE withSR #-}

arenaAllocate :: MSC (Ptr Pulse -> Ptr Pulse)
arenaAllocate = do
  sr <- MSC ask
  MSC $ lift $ tell $ Sum sr.samples
  (offset, memoTable) <- MSC $ lift $ lift get
  MSC $ lift $ lift $ put (offset + sr.samples, memoTable)
  pure (`plusPtr` (offset * pulseSize))
{-# INLINE arenaAllocate #-}

lookupMemoizedComputeSound :: SampleRate -> ComputationInfo -> MSC (Maybe (IO (Ptr Pulse)))
lookupMemoizedComputeSound sr ci = do
  (_, MemoComputeSound memoTable) <- MSC $ lift $ lift get
  res <- liftIO $ H.lookup memoTable (sr, ci)
  pure $ fmap (const $ fromJust <$> H.lookup memoTable (sr, ci)) res
{-# INLINE lookupMemoizedComputeSound #-}

memoizeComputeSound :: SampleRate -> ComputationInfo -> MSC (Ptr Pulse -> IO ())
memoizeComputeSound sr ci = do
  MemoComputeSound hashTable <- MSC $ lift $ lift $ snd <$> get
  liftIO $ H.insert hashTable (sr, ci) nullPtr
  pure $ \ptr -> H.insert hashTable (sr, ci) ptr
{-# INLINE memoizeComputeSound #-}

mVectorFromPtr :: Ptr Pulse -> Int -> IO (M.MVector M.RealWorld M.S Pulse)
mVectorFromPtr ptr len = do
  fPtr <- newForeignPtr_ ptr
  let mVector = VM.unsafeFromForeignPtr0 fPtr len
  pure $ M.fromStorableMVector mVector
{-# INLINE mVectorFromPtr #-}

vectorFromPtr :: Ptr Pulse -> Int -> IO (M.Vector M.S Pulse)
vectorFromPtr ptr len = do
  fPtr <- newForeignPtr_ ptr
  let vector = V.unsafeFromForeignPtr0 fPtr len
  pure $ M.fromStorableVector M.Seq vector
{-# INLINE vectorFromPtr #-}
