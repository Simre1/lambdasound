module LambdaSound.Sound
  ( -- ** Sound
    Sound (..),
    SoundDuration (..),
    Pulse (..),
    Duration (..),
    Progress (..),
    Percentage (..),
    SamplingInfo (..),
    Hz (..),
    Time (..),
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
    changeTempoM,

    -- ** Convolution
    Kernel (..),
    convolve,
    convolveDuration,
    modifyWholeSound,

    -- ** Making new sounds
    time,
    progress,
    sampleIndex,
    computeOnce,
    constant,
    silence,
  )
where

import Data.Coerce (coerce)
import Data.Foldable (foldl')
import Data.Functor ((<&>))
import Data.Massiv.Array qualified as M
import LambdaSound.Sound.ComputeSound (ComputeSound)
import LambdaSound.Sound.MSC
import LambdaSound.Sound.Types
import System.IO.Unsafe (unsafePerformIO)

-- | Some 'Sound's have a different while others do not.
-- 'I'nfinite 'Sound's have no duration.
-- 'T'imed 'Sound's have a duration.
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

mapComputation :: (MSC (ComputeSound a) -> MSC (ComputeSound b)) -> Sound d a -> Sound d b
mapComputation f (InfiniteSound msc) = InfiniteSound $ f msc
mapComputation f (TimedSound d msc) = TimedSound d $ f msc

instance Show (Sound d Pulse) where
  show (TimedSound d c) = showSampledCompute d c
  show (InfiniteSound c) = showSampledCompute 3 c

showSampledCompute :: Duration -> MSC (ComputeSound Pulse) -> String
showSampledCompute d msc = unsafePerformIO $ do
  let sr = makeSamplingInfo (coerce $ 25 / d) d
  floats <- sampleMSC sr msc
  pure $ show $ M.toList floats

instance Semigroup (Sound d Pulse) where
  -- \| Combines two sounds in a parallel manner (see 'parallel2')
  (<>) = parallel2

instance Monoid (Sound I Pulse) where
  mempty = silence

instance Monoid (Sound T Pulse) where
  mempty = TimedSound 0 $ makeIndexCompute (\_ _ -> 0)

instance (Num a) => Num (Sound I a) where
  (+) = zipSoundWith (+)
  (*) = zipSoundWith (*)
  (-) = zipSoundWith (-)
  abs = fmap abs
  fromInteger x = constant $ fromInteger x
  signum = fmap signum
  negate = fmap negate

instance Functor (Sound d) where
  fmap f = mapComputation $ mapComputeSound f

instance Applicative (Sound I) where
  pure = constant
  (<*>) = zipSoundWith ($)

-- | Append two sounds. This is only possible for sounds with a duration.
sequentially2 :: Sound T Pulse -> Sound T Pulse -> Sound T Pulse
sequentially2 (TimedSound d1 c1) (TimedSound d2 c2) =
  TimedSound (d1 + d2) $
    computeSequentially (coerce $ d1 / (d1 + d2)) c1 c2

-- | Same as 'sequentially2'
(>>>) :: Sound T Pulse -> Sound T Pulse -> Sound T Pulse
(>>>) = sequentially2

infixl 5 >>>

-- | Combine a list of sounds in a sequential manner.
sequentially :: [Sound T Pulse] -> Sound T Pulse
sequentially = foldl' sequentially2 mempty

-- | Get the time for each sample which can be used for sinus wave calculations (e.g. 'pulse')
time :: Sound I Time
time = InfiniteSound $ do
  makeIndexCompute $ \sr index ->
    coerce $ fromIntegral index * sr.period

-- | Get the 'Progress' of a 'Sound'.
-- 'Progress' of '0' means that the sound has just started
-- 'Progress' of '1' means that the sound has finished
-- 'Progress' greater than '1' or smaller than '0' is invalid
progress :: Sound I Progress
progress = InfiniteSound $ makeIndexCompute $ \sr index ->
  fromIntegral index / fromIntegral sr.samples

-- | Tells you the sample index for each sample
sampleIndex :: Sound I Int
sampleIndex = InfiniteSound $ makeIndexCompute $ \_ index -> index

-- | Combine two sounds such that they play in parallel
parallel2 :: Sound d Pulse -> Sound d Pulse -> Sound d Pulse
parallel2 (InfiniteSound c1) (InfiniteSound c2) = InfiniteSound $ zipWithCompute (+) c1 c2
parallel2 (TimedSound d1 c1) (TimedSound d2 c2) = TimedSound newDuration $ computeParallel longerC (coerce factor) shorterC
  where
    (longerC, factor, shorterC) =
      if d1 >= d2
        then (c1, d2 / newDuration, c2)
        else (c2, d1 / newDuration, c1)
    newDuration = max d1 d2

-- | Combine a lists of sounds such that they play in parallel
parallel :: (Monoid (Sound d Pulse)) => [Sound d Pulse] -> Sound d Pulse
parallel = foldl' parallel2 mempty

-- | A 'Sound' with '0' volume
silence :: Sound I Pulse
silence = constant 0

-- | A constant 'Sound'
constant :: a -> Sound I a
constant a = InfiniteSound $ makeIndexCompute $ \_ _ -> a

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

-- | Amplifies the volume of the given 'Sound'
amplify :: Float -> Sound d Pulse -> Sound d Pulse
amplify x = fmap (* coerce x)

-- | Reduces the volume of the given 'Sound'
reduce :: Float -> Sound d Pulse -> Sound d Pulse
reduce x = amplify (1 / x)

-- | Raises the frequency of the 'Sound' by the given factor.
-- Only works if the sound is based on some frequency (e.g. 'pulse' but not 'noise')
raise :: Float -> Sound d Pulse -> Sound d Pulse
raise x = mapComputation $ \msc -> do
  sr <- getSR
  withSR (sr {period = coerce x * sr.period}) msc

-- | Diminishes the frequency of the 'Sound' by the given factor
-- Only works if the sound is based on some frequency (e.g. 'pulse' but not 'noise')
diminish :: Float -> Sound d Pulse -> Sound d Pulse
diminish x = raise $ 1 / x

-- | Sets the duration of the 'Sound', scaling it
-- such that the previous sound fits within the resulting one.
-- The resuling sound is a 'T'imed 'Sound'.
setDuration :: Duration -> Sound d a -> Sound T a
setDuration d (TimedSound _ c) = TimedSound (max d 0) c
setDuration d (InfiniteSound c) = TimedSound (max d 0) c

-- | Same as `setDuration` but in operator form
(|->) :: Duration -> Sound d a -> Sound 'T a
(|->) = setDuration

infix 7 |->

-- | Scales the 'Duration' of a 'Sound'.
-- The following makes a sound twice as long:
-- > scaleDuration 2 sound
scaleDuration :: Float -> Sound T a -> Sound T a
scaleDuration x (TimedSound d c) = TimedSound (coerce x * d) c

-- | Get the duration of a 'T'imed 'Sound'
getDuration :: Sound T a -> Duration
getDuration (TimedSound d _) = d

-- | Reverses a 'Sound' similar to 'reverse' for lists
reverseSound :: Sound d a -> Sound d a
reverseSound = mapComputation $ modifyIndexCompute $ \sr oldCompute index ->
  oldCompute (pred sr.samples - index)

-- | Drop parts of a sound similar to 'drop' for lists
dropSound :: Duration -> Sound T a -> Sound T a
dropSound dropD' (TimedSound originalD msc) = TimedSound (originalD - dropD) $ do
  changeSampleRate (\sr -> sr {samples = paddedSamples sr}) $
    modifyIndexCompute
      (\sr f i -> f (i + paddedSamples sr - sr.samples))
      msc
  where
    dropD = max 0 $ min originalD dropD'
    droppedFactor = min 1 $ dropD / originalD
    factor = 1 - droppedFactor
    paddedSamples sr = if droppedFactor == 1 then 0 else round $ fromIntegral @_ @Float sr.samples * (1 / coerce factor)

-- | Take parts of a sound similar to 'take' for lists
takeSound :: Duration -> Sound T a -> Sound T a
takeSound takeD' (TimedSound originalD msc) =
  TimedSound takeD $
    changeSampleRate (\sr -> sr {samples = if takeD == 0 then 0 else round $ fromIntegral @_ @Float sr.samples * (1 / coerce factor)}) $
      modifyIndexCompute
        (\_ f -> f)
        msc
  where
    takeD = max 0 $ min takeD' originalD
    factor = takeD / originalD

-- | Change how the 'Sound' progresses. For example, you can slow it
-- down in the beginning and speed it up at the end. However, the total
-- duration stays the same.
--
-- Negative 'Progress' is treated as '0' and 'Progress' above '1' is treated as '1'
changeTempo :: (Progress -> Progress) -> Sound d a -> Sound d a
changeTempo f = mapComputation $ modifyIndexCompute $ \sr oldCompute index ->
  oldCompute $
    min sr.samples $
      round $
        f
          (fromIntegral index / fromIntegral sr.samples)
          * fromIntegral sr.samples

changeTempoM :: Sound I (Progress -> Progress) -> Sound d a -> Sound d a
changeTempoM (InfiniteSound msc1) =
  mapComputation $
    modifyIndexCompute2
      ( \sr progressCompute oldCompute index -> do
          f <- progressCompute index
          oldCompute $
            min sr.samples $
              round $
                f
                  (fromIntegral index / fromIntegral sr.samples)
                  * fromIntegral sr.samples
      )
      msc1

-- | A Kernel for convolution
data Kernel p = Kernel
  { coefficients :: Percentage -> Float,
    size :: p,
    offset :: p
  }

-- | Convolvution of a 'Sound' where the 'Kernel' size is
-- determined by 'Percentage's of the sound
convolve :: Kernel Percentage -> Sound d Pulse -> Sound d Pulse
convolve (Kernel coefficients sizeP offsetP) = mapComputation $ mapWholeComputation $ \wholeSound ->
  let n = M.unSz $ M.size wholeSound
      size = ceiling $ sizeP * fromIntegral n
      offset = round $ offsetP * fromIntegral n
      stencil = M.makeStencil (M.Sz1 size) offset $ \getV ->
        sum $ [0 .. pred size] <&> \i -> getV (i - offset) * (computedCoefficients M.! i)
      computedCoefficients =
        M.compute @M.S $
          if size <= 1
            then M.singleton 0.5
            else M.generate M.Seq (M.Sz1 size) $ \i ->
              coerce @_ @Pulse $
                coefficients (fromIntegral i / fromIntegral (size - 1))
   in M.mapStencil M.Reflect stencil wholeSound

-- | Convolution of a 'Sound' where the 'Kernel' size is
-- determined by a 'Duration'.
convolveDuration :: Kernel Duration -> Sound T Pulse -> Sound T Pulse
convolveDuration (Kernel coefficients sizeD offsetD) sound@(TimedSound d _) =
  convolve
    (Kernel coefficients (coerce $ sizeD / d) (coerce $ offsetD / d))
    sound

-- | Compute a value once and then reuse it while computing all samples
computeOnce :: (SamplingInfo -> a) -> Sound d (a -> b) -> Sound d b
computeOnce f = mapComputation $ modifyIndexCompute $ \sr ->
  let a = f sr
   in \oldCompute index -> ($ a) <$> oldCompute index

-- | Modify all samples of a sound so that you can look into the past and future
-- of a sound (e.g. IIR filter).
modifyWholeSound :: (M.Load r M.Ix1 Pulse) => (M.Vector M.S Pulse -> M.Vector r Pulse) -> Sound d Pulse -> Sound d Pulse
modifyWholeSound f = mapComputation $ mapWholeComputation f