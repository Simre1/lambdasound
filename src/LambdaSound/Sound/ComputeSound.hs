module LambdaSound.Sound.ComputeSound where

import Control.Monad (join)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.HashTable.IO qualified as H
import Data.Hashable
import Data.Massiv.Array qualified as M
import Data.Massiv.Array.Unsafe qualified as MU
import Data.SomeStableName (SomeStableName, makeSomeStableName)
import Foreign.ForeignPtr (withForeignPtr)
import Foreign.Marshal (copyBytes)
import Foreign.Storable (Storable (..))
import GHC.Generics (Generic)
import LambdaSound.Sound.Types

makeWithIndexFunction :: (SamplingInfo -> Int -> a) -> ComputeSound a
makeWithIndexFunction f = makeDelayedResult $ \si ->
  let f' = f si
   in M.makeArray M.Seq (M.Sz1 si.samples) f'
{-# INLINE makeWithIndexFunction #-}

makeDelayedResult :: (SamplingInfo -> M.Vector M.D a) -> ComputeSound a
makeDelayedResult f = ComputeSound $ \si _ -> do
  stableF <- makeSomeStableName f
  pure (DelayedResult $ pure $ f si, ComputationInfoMakeDelayedResult stableF)
{-# INLINE makeDelayedResult #-}

changeSamplingInfo :: (SamplingInfo -> SamplingInfo) -> ComputeSound a -> ComputeSound a
changeSamplingInfo changeSI (ComputeSound compute) = ComputeSound $ \si memo -> do
  stableChangeSI <- makeSomeStableName changeSI
  (result, ci) <- compute (changeSI si) memo
  pure (result, ComputationInfoChangeSamplingInfo stableChangeSI ci)
{-# INLINE changeSamplingInfo #-}

mapDelayedResult :: (SamplingInfo -> M.Vector M.D a -> M.Vector M.D b) -> ComputeSound a -> ComputeSound b
mapDelayedResult mapVector cs = ComputeSound $ \si memo -> do
  (delayedVector, ci) <- asDelayedResult cs si memo
  stableMapVector <- makeSomeStableName mapVector
  let mapVector' = mapVector si
  pure (DelayedResult $ fmap mapVector' delayedVector, ComputationInfoMapDelayedResult stableMapVector ci)
{-# INLINE mapDelayedResult #-}

withSamplingInfoCS :: (SamplingInfo -> ComputeSound a) -> ComputeSound a
withSamplingInfoCS f = ComputeSound $ \si memo -> do
  stableF <- makeSomeStableName f
  let (ComputeSound compute) = f si
  (res, _) <- compute si memo
  pure (res, ComputationInfoWithSamplingInfo stableF)
{-# INLINE withSamplingInfoCS #-}

withSampledSoundPulseCS :: Duration -> ComputeSound Pulse -> (M.Vector M.S Pulse -> ComputeSound a) -> ComputeSound a
withSampledSoundPulseCS duration cs f = ComputeSound $ \si memo -> do
  let sampleSI = makeSamplingInfo si.sampleRate duration
  (writeSamples, ci) <- asWriteResult cs sampleSI memo
  dest <- MU.unsafeMallocMArray (M.Sz1 sampleSI.samples)
  writeSamples dest
  samples <- MU.unsafeFreeze M.Seq dest
  let (ComputeSound compute) = f samples
  (res, _) <- compute si memo
  stableF <- makeSomeStableName f
  pure (res, ComputationInfoWithSampledSound stableF ci)
{-# INLINE withSampledSoundPulseCS #-}

withSampledSoundCS :: Duration -> ComputeSound a -> (M.Vector M.D a -> ComputeSound b) -> ComputeSound b
withSampledSoundCS duration cs f = ComputeSound $ \si memo -> do
  let sampleSI = makeSamplingInfo si.sampleRate duration
  (delayedVector, ci) <- asDelayedResult cs sampleSI memo
  let nextCS = f <$> delayedVector
  stableF <- makeSomeStableName f
  pure
    ( DelayedResult $ do
        (finalDelayedVector, _) <- join $ asDelayedResult <$> nextCS <*> pure si <*> pure memo
        finalDelayedVector,
      ComputationInfoWithSampledSound stableF ci
    )
{-# INLINE withSampledSoundCS #-}

mergeDelayedResult :: (SamplingInfo -> M.Vector M.D a -> M.Vector M.D b -> M.Vector M.D c) -> ComputeSound a -> ComputeSound b -> ComputeSound c
mergeDelayedResult merge cs1 cs2 = ComputeSound $ \si memo -> do
  stableMerge <- makeSomeStableName merge
  (delayedResult1, ci1) <- asDelayedResult cs1 si memo
  (delayedResult2, ci2) <- asDelayedResult cs2 si memo
  let merge' = merge si
  pure (DelayedResult $ merge' <$> delayedResult1 <*> delayedResult2, ComputationInfoMergeDelayedResult stableMerge ci1 ci2)
{-# INLINE mergeDelayedResult #-}

computeSequentially :: Percentage -> ComputeSound Pulse -> ComputeSound Pulse -> ComputeSound Pulse
computeSequentially factor c1 c2 = ComputeSound $ \si memo -> do
  let splitIndex =
        round $
          factor * fromIntegral si.samples
  (writeResult1, ci1) <- asWriteResult c1 si {samples = splitIndex} memo
  (writeResult2, ci2) <- asWriteResult c2 si {samples = si.samples - splitIndex} memo
  pure
    ( WriteResult $ \dest -> do
        writeResult1 $ MU.unsafeLinearSliceMArray 0 (M.Sz1 splitIndex) dest
        writeResult2 $ MU.unsafeLinearSliceMArray splitIndex (M.Sz1 $ si.samples - splitIndex) dest,
      ComputationInfoSequentially factor ci1 ci2
    )
{-# INLINE computeSequentially #-}

computeParallel :: ComputeSound Pulse -> Percentage -> ComputeSound Pulse -> ComputeSound Pulse
computeParallel c1 factor c2 = ComputeSound $ \si memo -> do
  let c2N = round $ factor * fromIntegral si.samples
  (delayedResult1, p1) <- asDelayedResult c1 si memo
  (delayedResult2, p2) <- asDelayedResult c2 si {samples = c2N} memo
  pure
    ( if si.samples == c2N
        then DelayedResult $ M.zipWith (+) <$> delayedResult1 <*> delayedResult2
        else DelayedResult $ do
          dR1 <- delayedResult1
          dR2 <- delayedResult2
          pure $ M.imap (\index -> (+) $ if index < c2N then MU.unsafeIndex dR2 index else 0) dR1,
      ComputationInfoParallel factor p1 p2
    )
{-# INLINE computeParallel #-}

mapComputeSound :: (a -> b) -> ComputeSound a -> ComputeSound b
mapComputeSound f cs = ComputeSound $ \si memo -> do
  stableF <- makeSomeStableName f
  (result, ci) <- asDelayedResult cs si memo
  pure (DelayedResult $ M.map f <$> result, ComputationInfoMap stableF ci)
{-# INLINE mapComputeSound #-}

asDelayedResult ::
  ComputeSound a ->
  SamplingInfo ->
  MemoComputeSound ->
  IO (IO (M.Vector M.D a), ComputationInfo)
asDelayedResult (ComputeSound compute) si memo = do
  (result, ci) <- compute si memo
  case result of
    DelayedResult vector -> pure (vector, ci)
    WriteResult writeResult ->
      pure
        ( do
            marray <- MU.unsafeMallocMArray (M.Sz1 si.samples)
            writeResult marray
            array <- MU.unsafeFreeze M.Seq marray
            pure $ M.delay array,
          ci
        )
{-# INLINE asDelayedResult #-}

asWriteResult ::
  ComputeSound Pulse ->
  SamplingInfo ->
  MemoComputeSound ->
  IO (M.MVector M.RealWorld M.S Pulse -> IO (), ComputationInfo)
asWriteResult (ComputeSound compute) si memo = do
  (result, ci) <- compute si memo
  case result of
    WriteResult writeResult -> pure (writeResult, ci)
    DelayedResult vector -> do
      let memoInfo = MemoInfo si ci
      pure
        ( \dest -> do
            memoized <- lookupMemoizedComputeSound memo memoInfo

            case memoized of
              Just memoSource -> do
                copyArrayIntoMArray memoSource dest
              Nothing -> do
                vector >>= M.computeInto dest
                destArray <- MU.unsafeFreeze M.Seq dest
                memoizeComputeSound memo memoInfo destArray,
          ci
        )
{-# INLINE asWriteResult #-}

zipWithCompute :: (a -> b -> c) -> ComputeSound a -> ComputeSound b -> ComputeSound c
zipWithCompute f cs1 cs2 = ComputeSound $ \si memo -> do
  (dV1, p1) <- asDelayedResult cs1 si memo
  (dV2, p2) <- asDelayedResult cs2 si memo
  stableF <- makeSomeStableName f
  pure (DelayedResult $ M.zipWith f <$> dV1 <*> dV2, ComputationInfoZip stableF p1 p2)
{-# INLINE zipWithCompute #-}

mapSoundFromMemory :: (M.Load r M.Ix1 Pulse) => (M.Vector M.S Pulse -> M.Vector r Pulse) -> ComputeSound Pulse -> ComputeSound Pulse
mapSoundFromMemory f cs = ComputeSound $ \si memo -> do
  (writeSamples, ci) <- asWriteResult cs si memo
  stableF <- makeSomeStableName f
  pure
    ( WriteResult $ \dest -> do
        wholeSoundMArray <- MU.unsafeMallocMArray (M.Sz1 si.samples)
        writeSamples wholeSoundMArray
        wholeSoundArray <- MU.unsafeFreeze M.Seq wholeSoundMArray
        M.computeInto dest $ f wholeSoundArray,
      ComputationInfoMapMemory stableF ci
    )
{-# INLINE mapSoundFromMemory #-}

mapSoundFromMemoryIO :: (M.Vector M.S Pulse -> M.MVector M.RealWorld M.S Pulse -> IO ()) -> ComputeSound Pulse -> ComputeSound Pulse
mapSoundFromMemoryIO f cs = ComputeSound $ \si memo -> do
  (writeSamples, ci) <- asWriteResult cs si memo
  stableF <- makeSomeStableName f
  pure
    ( WriteResult $ \dest -> do
        wholeSoundMArray <- MU.unsafeMallocMArray (M.Sz1 si.samples)
        writeSamples wholeSoundMArray
        wholeSoundArray <- MU.unsafeFreeze M.Seq wholeSoundMArray
        f wholeSoundArray dest,
      ComputationInfoMapMemory stableF ci
    )
{-# INLINE mapSoundFromMemoryIO #-}

fillSoundInMemoryIO :: (SamplingInfo -> M.MVector M.RealWorld M.S Pulse -> IO ()) -> ComputeSound Pulse
fillSoundInMemoryIO f = ComputeSound $ \si _ -> do
  stableF <- makeSomeStableName f
  let f' = f si
  pure
    ( WriteResult $ \dest -> do
        f' dest,
      ComputationInfoFillMemory stableF
    )
{-# INLINE fillSoundInMemoryIO #-}

embedIOCS :: IO (ComputeSound a) -> ComputeSound a
embedIOCS makeCS = ComputeSound $ \si memo -> do
  stableIO <- makeSomeStableName makeCS
  (ComputeSound compute) <- makeCS
  (res, _) <- compute si memo
  pure (res, ComputationInfoIO stableIO)
{-# INLINE embedIOCS #-}

embedIOLazilyCS :: IO (ComputeSound a) -> ComputeSound a
embedIOLazilyCS makeCS = ComputeSound $ \si memo -> do
  stableIO <- makeSomeStableName makeCS
  pure
    ( DelayedResult $ do
        (res, _) <- join $ asDelayedResult <$> makeCS <*> pure si <*> pure memo
        res,
      ComputationInfoIO stableIO
    )
{-# INLINE embedIOLazilyCS #-}

pulseSize :: Int
pulseSize = sizeOf (undefined :: Pulse)
{-# INLINE pulseSize #-}

sampleComputeSound :: SamplingInfo -> ComputeSound Pulse -> IO (M.Vector M.S Pulse)
sampleComputeSound si cs = do
  hashTable <- H.new
  destArray <- MU.unsafeMallocMArray $ M.Sz1 si.samples
  (writeResult, _) <- asWriteResult cs si (MemoComputeSound hashTable)
  writeResult destArray
  MU.unsafeFreeze M.Seq destArray
{-# INLINE sampleComputeSound #-}

newtype MemoComputeSound = MemoComputeSound (H.BasicHashTable MemoInfo (M.Vector M.S Pulse))

data MemoInfo = MemoInfo
  { samplingInfo :: !SamplingInfo,
    computationInfo :: !ComputationInfo
  }
  deriving (Eq, Generic)

instance Hashable MemoInfo

lookupMemoizedComputeSound :: MemoComputeSound -> MemoInfo -> IO (Maybe (M.Vector M.S Pulse))
lookupMemoizedComputeSound (MemoComputeSound memoTable) memoInfo = do
  H.lookup memoTable memoInfo
{-# INLINE lookupMemoizedComputeSound #-}

memoizeComputeSound :: MemoComputeSound -> MemoInfo -> M.Vector M.S Pulse -> IO ()
memoizeComputeSound (MemoComputeSound hashTable) memoInfo vec = do
  H.insert hashTable memoInfo vec
{-# INLINE memoizeComputeSound #-}

newtype ComputeSound a = ComputeSound
  { compute ::
      SamplingInfo ->
      MemoComputeSound ->
      IO (SoundResult a, ComputationInfo)
  }

data SoundResult a where
  WriteResult :: (M.MVector M.RealWorld M.S Pulse -> IO ()) -> SoundResult Pulse
  DelayedResult :: IO (M.Vector M.D a) -> SoundResult a

data ComputationInfo
  = ComputationInfoZip SomeStableName ComputationInfo ComputationInfo
  | ComputationInfoMap SomeStableName ComputationInfo
  | ComputationInfoSequentially Percentage ComputationInfo ComputationInfo
  | ComputationInfoParallel Percentage ComputationInfo ComputationInfo
  | ComputationInfoMakeDelayedResult SomeStableName
  | ComputationInfoMapDelayedResult SomeStableName ComputationInfo
  | ComputationInfoMergeDelayedResult SomeStableName ComputationInfo ComputationInfo
  | ComputationInfoMapMemory SomeStableName ComputationInfo
  | ComputationInfoFillMemory SomeStableName
  | ComputationInfoChangeSamplingInfo SomeStableName ComputationInfo
  | ComputationInfoWithSampledSound SomeStableName ComputationInfo
  | ComputationInfoIO SomeStableName
  | ComputationInfoWithSamplingInfo SomeStableName
  deriving (Eq, Generic, Show)

instance Hashable ComputationInfo

copyArrayIntoMArray :: M.Vector M.S Pulse -> M.MVector M.RealWorld M.S Pulse -> IO ()
copyArrayIntoMArray source dest =
  let (sourceFPtr, _) = MU.unsafeArrayToForeignPtr source
      (destFPtr, _) = MU.unsafeMArrayToForeignPtr dest
   in liftIO $ withForeignPtr sourceFPtr $ \sourcePtr ->
        withForeignPtr destFPtr $ \destPtr ->
          copyBytes destPtr sourcePtr (M.unSz (M.size source) * pulseSize)
{-# INLINE copyArrayIntoMArray #-}

copyMArrayIntoMArray :: M.MVector M.RealWorld M.S Pulse -> M.MVector M.RealWorld M.S Pulse -> IO ()
copyMArrayIntoMArray source dest =
  let (sourceFPtr, _) = MU.unsafeMArrayToForeignPtr source
      (destFPtr, _) = MU.unsafeMArrayToForeignPtr dest
   in liftIO $ withForeignPtr sourceFPtr $ \sourcePtr ->
        withForeignPtr destFPtr $ \destPtr ->
          copyBytes destPtr sourcePtr (M.unSz (M.sizeOfMArray source) * pulseSize)
{-# INLINE copyMArrayIntoMArray #-}
