{-# LANGUAGE LambdaCase #-}

module LambdaSound.Sound2
  ( -- ** Sound
    Sound (..),
    SoundDuration (..),
    Pulse (..),
    Duration (..),
    Progress (..),
    Percentage (..),
    SampleRate (..),
    CurrentSample (..),
    DetermineDuration,

    -- ** Combinators
    sequentially2,
    (>>>),
    sequentially,
    parallel2,
    parallel,
    zipSoundWith,
    amplify,
    reduce,
    raise,
    diminish,
    setDuration,
    (|->),
    getDuration,
    scaleDuration,
    reverseSound,
    dropSound,
    takeSound,
    changeTempo,

    -- ** Convolution
    Kernel (..),
    convolve,
    convolveDuration,

    -- ** Making new sounds
    time,
    progress,
    sampleIndex,
    computeOnce,
    constant,
    silence,
  )
where

import Control.DeepSeq (NFData)
import Control.Monad (forM_)
import Control.Monad.IO.Class
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Reader
import Control.Monad.Trans.State.Strict
import Control.Monad.Trans.Writer.CPS
import Data.Coerce (coerce)
import Data.Foldable (foldl')
import Data.Monoid (Sum (..))
import Foreign.Ptr
import Foreign.Storable

-- | An audio sample
newtype Pulse = Pulse Float deriving (Show, Eq, Floating, Num, Fractional, Ord, Real, RealFrac, NFData, Storable)

-- | The duration of a 'Sound'
newtype Duration = Duration Float deriving (Show, Eq, Floating, Num, Fractional, Ord, Real, RealFrac, NFData, Storable)

-- | The progress of a 'Sound'. A sound progresses from '0' to '1'
-- while it plays.
newtype Progress = Progress Float deriving (Show, Eq, Floating, Num, Fractional, Ord, Real, RealFrac, NFData, Storable)

-- | The percentage of a 'Sound'. '0.3' corresponds to 30% of a 'Sound'.
newtype Percentage = Percentage Float deriving (Show, Eq, Floating, Num, Fractional, Ord, Real, RealFrac, NFData, Storable)

-- | Gives information about how many samples are needed during computation
data SampleRate = SampleRate
  { period :: !Float,
    samples :: !Int
  }
  deriving (Show)

-- | Contains the index of the sample currently being computed
newtype CurrentSample = CurrentSample
  { index :: Int
  }
  deriving (Show)

-- | Some 'Sound's have a different while others do not.
-- 'I'nfinite 'Sound's have no duration
-- 'T'imed 'Sound's have a duration
data SoundDuration = I | T

-- | Determines the duration of two sounds when they are combined
type family DetermineDuration (d1 :: SoundDuration) (d2 :: SoundDuration) where
  DetermineDuration I d = d
  DetermineDuration d I = d
  DetermineDuration T _ = T
  DetermineDuration _ T = T

data Sound (d :: SoundDuration) a where
  TimedSound ::
    !Duration ->
    MSC (ComputeSound a) ->
    Sound T a
  InfiniteSound ::
    MSC (ComputeSound a) ->
    Sound I a

instance Semigroup (Sound d Pulse) where
  -- \| Combines two sounds in a parallel manner (see 'parallel2')
  (<>) = parallel2
  {-# INLINE (<>) #-}

instance Monoid (Sound I Pulse) where
  mempty = silence
  {-# INLINE mempty #-}

instance Monoid (Sound T Pulse) where
  mempty = TimedSound 0 $ pure $ IndexCompute $ pure (const $ pure 0)
  {-# INLINE mempty #-}

instance (Num a) => Num (Sound I a) where
  (+) = zipSoundWith (+)
  (*) = zipSoundWith (*)
  (-) = zipSoundWith (-)
  abs = fmap abs
  fromInteger x = constant $ fromInteger x
  signum = fmap signum
  negate = fmap negate
  {-# INLINE (+) #-}
  {-# INLINE (-) #-}
  {-# INLINE (*) #-}
  {-# INLINE abs #-}
  {-# INLINE fromInteger #-}
  {-# INLINE signum #-}
  {-# INLINE negate #-}

instance Functor (Sound d) where
  fmap f = mapComputation $ mapComputeSound f

-- | Append two sounds. This is only possible for sounds with a duration.
sequentially2 :: Sound T Pulse -> Sound T Pulse -> Sound T Pulse
sequentially2 (TimedSound d1 c1) (TimedSound d2 c2) = TimedSound (d1 + d2) $ do
  sr <- getSR
  let factor = d1 / (d1 + d2)
      splitIndex =
        round $ factor * fromIntegral sr.samples
  w1 <- withSR (sr {samples = splitIndex}) $ asWriteMemory c1
  w2 <- withSR (sr {samples = sr.samples - splitIndex}) $ asWriteMemory c2
  pure $ WriteMemory $ \ptr -> w1 ptr >> w2 (ptr `plusPtr` splitIndex)
{-# INLINE sequentially2 #-}

-- | Same as 'sequentially2'
(>>>) :: Sound T Pulse -> Sound T Pulse -> Sound T Pulse
(>>>) = sequentially2

-- | Combine a list of sounds in a sequential manner.
sequentially :: [Sound T Pulse] -> Sound T Pulse
sequentially = foldl' sequentially2 mempty
{-# INLINE sequentially #-}

-- | Get the time for each sample which can be used for sinus wave calculations (e.g. 'pulse')
time :: Sound I Float
time = InfiniteSound $ do
  sr <- getSR
  pure $ IndexCompute $ pure $ \index ->
    pure $ fromIntegral index * sr.period
{-# INLINE time #-}

-- | Get the 'Progress' of a 'Sound'.
-- 'Progress' of '0' means that the sound has just started
-- 'Progress' of '1' means that the sound has finished
-- 'Progress' greater than '1' or smaller than '0' is invalid
progress :: Sound I Progress
progress = InfiniteSound $ do
  sr <- getSR
  pure $ IndexCompute $ pure $ \index ->
    pure $ fromIntegral index / fromIntegral sr.samples
{-# INLINE progress #-}

-- | Tells you the sample index for each sample
sampleIndex :: Sound I Int
sampleIndex = InfiniteSound $ pure $ IndexCompute $ pure pure
{-# INLINE sampleIndex #-}

-- | Combine two sounds such that they play in parallel
parallel2 :: Sound d Pulse -> Sound d Pulse -> Sound d Pulse
parallel2 (InfiniteSound c1) (InfiniteSound c2) = InfiniteSound $ zipWithCompute (+) c1 c2
parallel2 (TimedSound d1 c1) (TimedSound d2 c2) = TimedSound newDuration $ do
  sr <- getSR
  let d1N = round $ d1Percentage * fromIntegral sr.samples
      d2N = round $ d2Percentage * fromIntegral sr.samples
  ic1 <- withSR (sr {samples = d1N}) $ asIndexCompute c1
  ic2 <- withSR (sr {samples = d2N}) $ asIndexCompute c2
  pure $ IndexCompute $ do
    ic1' <- ic1
    ic2' <- ic2
    if d1 >= d2
      then pure $ \index -> (+) <$> ic1' index <*> if index < d2N then ic2' index else pure 0
      else pure $ \index -> (+) <$> ic2' index <*> if index < d1N then ic1' index else pure 0
  where
    d1Percentage = d1 / newDuration
    d2Percentage = d2 / newDuration
    newDuration = max d1 d2

-- | Combine a lists of sounds such that they play in parallel
parallel :: (Monoid (Sound d Pulse)) => [Sound d Pulse] -> Sound d Pulse
parallel = foldl' parallel2 mempty
{-# INLINE parallel #-}

-- | A 'Sound' with '0' volume
silence :: Sound I Pulse
silence = constant 0
{-# INLINE silence #-}

-- | A constant 'Sound'
constant :: a -> Sound I a
constant a = InfiniteSound $ pure $ IndexCompute $ pure $ const $ pure a

-- | Zip two 'Sound's. The duration of the resulting 'Sound' is equivalent
-- to the duration of the shorter 'Sound', cutting away the excess samples from the longer one.
zipSoundWith :: (a -> b -> c) -> Sound d1 a -> Sound d2 b -> Sound (DetermineDuration d1 d2) c
zipSoundWith f sound1 sound2 =
  case (sound1, sound2) of
    (TimedSound d1 _, TimedSound d2 _) ->
      let d = min d1 d2
       in case (takeSound d sound1, takeSound d sound2) of
            (TimedSound _ c1, TimedSound _ c2) -> TimedSound d $ zipWithCompute f c1 c2
    (TimedSound d c1, InfiniteSound c2) -> TimedSound d $ zipWithCompute f c1 c2
    (InfiniteSound c1, TimedSound d c2) -> TimedSound d $ zipWithCompute f c1 c2
    (InfiniteSound c1, InfiniteSound c2) -> InfiniteSound $ zipWithCompute f c1 c2
{-# INLINE zipSoundWith #-}

-- | Amplifies the volume of the given 'Sound'
amplify :: Float -> Sound d Pulse -> Sound d Pulse
amplify x = fmap (* coerce x)
{-# INLINE amplify #-}

-- | Reduces the volume of the given 'Sound'
reduce :: Float -> Sound d Pulse -> Sound d Pulse
reduce x = amplify (1 / x)
{-# INLINE reduce #-}

-- | Raises the frequency of the 'Sound' by the given factor.
-- Only works if the sound is based on some frequency (e.g. 'pulse' but not 'noise')
raise :: Float -> Sound d Pulse -> Sound d Pulse
raise x = mapComputation $ \msc -> do
  sr <- getSR
  withSR (sr {period = coerce x * sr.period}) msc
{-# INLINE raise #-}

-- | Diminishes the frequency of the 'Sound' by the given factor
-- Only works if the sound is based on some frequency (e.g. 'pulse' but not 'noise')
diminish :: Float -> Sound d Pulse -> Sound d Pulse
diminish x = raise $ 1 / x
{-# INLINE diminish #-}

-- | Sets the duration of the 'Sound', scaling it
-- such that the previous sound fits within the resulting one.
-- The resuling sound is a 'T'imed 'Sound'.
setDuration :: Duration -> Sound d a -> Sound T a
setDuration d (TimedSound _ c) = TimedSound d c
setDuration d (InfiniteSound c) = TimedSound d c
{-# INLINE setDuration #-}

-- | Same as `setDuration` but in operator form
(|->) :: Duration -> Sound d a -> Sound 'T a
(|->) = setDuration
{-# INLINE (|->) #-}

-- | Scales the 'Duration' of a 'Sound'.
-- The following makes a sound twice as long:
-- > scaleDuration 2 sound
scaleDuration :: Float -> Sound T a -> Sound T a
scaleDuration x (TimedSound d c) = TimedSound (coerce x * d) c
{-# INLINE scaleDuration #-}

-- | Get the duration of a 'T'imed 'Sound'
getDuration :: Sound T a -> Duration
getDuration (TimedSound d _) = d
{-# INLINE getDuration #-}

-- | Reverses a 'Sound' similar to 'reverse' for lists
reverseSound :: Sound d a -> Sound d a
reverseSound = mapComputation $ \msc -> do
  sr <- getSR
  ic <- asIndexCompute msc
  pure $ IndexCompute $ (\ic' -> ic' . (pred sr.samples -)) <$> ic
{-# INLINE reverseSound #-}

-- | Drop parts of a sound similar to 'drop' for lists
dropSound :: Duration -> Sound T a -> Sound T a
dropSound dropD' (TimedSound originalD c) = undefined -- TimedSound (originalD - dropD) $ \sr ->
--   let c' = c sr {samples = paddedSamples sr}
--    in \cs ->
--         c' $
--           let !x = cs {index = cs.index + paddedSamples sr - sr.samples}
--            in x
--   where
--     dropD = max 0 $ min originalD dropD'
--     droppedFactor = dropD / originalD
--     factor = 1 - droppedFactor
--     paddedSamples sr = round $ fromIntegral @_ @Float sr.samples * (1 / coerce factor)
-- {-# INLINE dropSound #-}

-- | Take parts of a sound similar to 'take' for lists
takeSound :: Duration -> Sound T a -> Sound T a
takeSound takeD' (TimedSound originalD c) = undefined -- TimedSound takeD $ \sr ->
-- let c' = c sr {samples = round $ fromIntegral @_ @Float sr.samples * (1 / coerce factor)}
--  in c'
-- where
--   takeD = max 0 $ min takeD' originalD
--   factor = takeD / originalD
{-# INLINE takeSound #-}

-- | Change how the 'Sound' progresses. For example, you can slow it
-- down in the beginning and speed it up at the end. However, the total
-- duration stays the same.
--
-- Negative 'Progress' is treated as '0' and 'Progress' above '1' is treated as '1'
changeTempo :: (Progress -> Progress) -> Sound d a -> Sound d a
changeTempo f = mapComputation $ \msc -> do
  sr <- getSR
  ic <- asIndexCompute msc
  pure $ IndexCompute $ (\ic' -> ic' . changeIndex sr) <$> ic
  where
    changeIndex sr index =
      min sr.samples $
        round $
          f
            (fromIntegral index / fromIntegral sr.samples)
            * fromIntegral sr.samples
{-# INLINE changeTempo #-}

mapComputation :: (MSC (ComputeSound a) -> MSC (ComputeSound b)) -> Sound d a -> Sound d b
mapComputation f (InfiniteSound msc) = InfiniteSound $ f msc
mapComputation f (TimedSound d msc) = TimedSound d $ f msc

newtype MSC a = MSC (ReaderT SampleRate (WriterT (Sum Int) (StateT (Ptr Pulse) IO)) a) deriving (Functor, Applicative, Monad, MonadIO)

getSR :: MSC SampleRate
getSR = MSC ask

withSR :: SampleRate -> MSC a -> MSC a
withSR sr (MSC r) = MSC $ local (const sr) r

arenaAllocate :: MSC (Ptr Pulse)
arenaAllocate = do
  sr <- MSC ask
  MSC $ lift $ tell $ Sum sr.samples
  ptr <- MSC $ lift $ lift get
  MSC $ lift $ lift $ put (ptr `plusPtr` sr.samples)
  pure ptr

mapComputeSound :: (a -> b) -> MSC (ComputeSound a) -> MSC (ComputeSound b)
mapComputeSound f cs = IndexCompute . fmap (fmap f .) <$> asIndexCompute cs

asIndexCompute :: MSC (ComputeSound a) -> MSC (IO (Int -> IO a))
asIndexCompute msc =
  msc >>= \case
    IndexCompute f -> pure f
    WriteMemory w -> do
      ptr <- arenaAllocate
      pure $ do
        w ptr
        pure $ \index -> peek (ptr `plusPtr` index)

asWriteMemory :: MSC (ComputeSound Pulse) -> MSC (Ptr Pulse -> IO ())
asWriteMemory mcs =
  mcs >>= \case
    IndexCompute ic -> do
      sr <- MSC ask
      pure $ \ptr -> do
        ic' <- ic
        forM_ [0 .. pred sr.samples] $ \i ->
          ic' i >>= poke (ptr `plusPtr` i)
    WriteMemory w -> pure w

data ComputeSound a where
  WriteMemory :: (Ptr Pulse -> IO ()) -> ComputeSound Pulse
  IndexCompute :: (IO (Int -> IO a)) -> ComputeSound a

zipWithCompute :: (a -> b -> c) -> MSC (ComputeSound a) -> MSC (ComputeSound b) -> MSC (ComputeSound c)
zipWithCompute f msc1 msc2 = do
  ic1 <- asIndexCompute msc1
  ic2 <- asIndexCompute msc2
  pure $ IndexCompute $ do
    ic1' <- ic1
    ic2' <- ic2
    pure $ \i -> f <$> ic1' i <*> ic2' i

-- | A Kernel for convolution
data Kernel p = Kernel
  { coefficients :: Percentage -> Float,
    size :: p,
    offset :: p
  }

-- | Convolvution of a 'Sound' where the 'Kernel' size is
-- determined by 'Percentage's of the sound
convolve :: Kernel Percentage -> Sound d Pulse -> Sound d Pulse
convolve (Kernel coefficients sizeP offsetP) = undefined -- mapComputation $ \c sr ->
--   let size = ceiling $ sizeP * fromIntegral sr.samples
--       offset = round $ offsetP * fromIntegral sr.samples

--       c' = c sr
--       wholeSound = M.compute @M.S $ M.generate M.Seq (M.Sz1 sr.samples) $ c' . CurrentSample

--       stencil = M.makeStencil (M.Sz1 size) offset $ \get ->
--         sum $ [0 .. pred size] <&> \i -> get (i - offset) * (computedCoefficients M.! i)
--       computedCoefficients =
--         M.compute @M.S $
--           if size <= 1
--             then M.singleton 0.5
--             else M.generate M.Seq (M.Sz1 size) $ \i ->
--               coerce @_ @Pulse $
--                 coefficients (fromIntegral i / fromIntegral (size - 1))

--       convolved = M.compute @M.S $ M.mapStencil M.Reflect stencil wholeSound
--    in \cs -> convolved M.! cs.index
-- {-# INLINE convolve #-}

-- | Convolution of a 'Sound' where the 'Kernel' size is
-- determined by a 'Duration'.
convolveDuration :: Kernel Duration -> Sound T Pulse -> Sound T Pulse
convolveDuration (Kernel coefficients sizeD offsetD) sound@(TimedSound d _) =
  convolve
    (Kernel coefficients (coerce $ sizeD / d) (coerce $ offsetD / d))
    sound
{-# INLINE convolveDuration #-}

-- | Compute a value once and then reuse it while computing all samples
computeOnce :: (SampleRate -> a) -> Sound d (a -> b) -> Sound d b
computeOnce f = mapComputation $ \msc -> do
  sr <- getSR
  ic <- asIndexCompute msc
  pure $ IndexCompute $ do
    let a = f sr
    fmap (fmap ($ a) .) ic
{-# INLINE computeOnce #-}

-- | Compute a whole sound so that you can look into the past and future
-- of a sound (e.g. IIR filter).
-- computeWholeSound :: (M.Vector M.D a -> CurrentSample -> b) -> Sound d a -> Sound d b
-- computeWholeSound f = mapComputation $ \c sr ->
--   let c' = c sr
--       a = M.generate M.Seq (M.Sz1 sr.samples) $ c' . CurrentSample
--    in f a
