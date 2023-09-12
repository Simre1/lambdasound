module LambdaSound.Sound
  ( -- ** Sound types
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

    -- ** Basic sounds
    time,
    progress,
    sampleIndex,
    constant,
    silence,

    -- ** Sounds in sequence
    timedSequentially,
    (>>>),
    sequentially,
    infiniteSequentially,

    -- ** Sounds in parallel
    parallel2,
    parallel,

    -- ** Volume
    amplify,
    reduce,

    -- ** Pitch
    raise,
    diminish,

    -- ** Duration
    setDuration,
    (|->),
    getDuration,
    scaleDuration,
    dropDuration,
    adoptDuration,

    -- ** Sample order
    reverseSound,
    dropSound,
    takeSound,

    -- ** Zipping
    zipSoundWith,
    zipSound,

    -- ** Convolution
    Kernel (..),
    convolve,
    convolveDuration,

    -- ** Change play behavior of a sound
    changeTempo,
    changeTempoM,

    -- ** Modify the samples of a sound
    modifyWholeSound,
    modifyWholeSoundST,
    fillWholeSound,
    fillWholeSoundST,
    computeOnce,
    withSamplingInfo,
  )
where

import Control.Monad.ST
import Data.Coerce (coerce)
import Data.Foldable (foldl')
import Data.Massiv.Array qualified as M
import Data.Massiv.Array.Unsafe qualified as MU
import LambdaSound.Sound.ComputeSound
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
    ComputeSound a ->
    Sound T a
  InfiniteSound ::
    ComputeSound a ->
    Sound I a

-- data SoundType d where
--   InfiniteSoundType :: SoundType I
--   TimedSoundType :: SoundType T

-- class DetermineSoundType d where
--   determineSoundType :: SoundType d

-- instance DetermineSoundType I where
--   determineSoundType = InfiniteSoundType

-- instance DetermineSoundType T where
--   determineSoundType = TimedSoundType

getCS :: Sound d a -> ComputeSound a
getCS (InfiniteSound cs) = cs
getCS (TimedSound _ cs) = cs

mapComputation :: (ComputeSound a -> ComputeSound b) -> Sound d a -> Sound d b
mapComputation f (InfiniteSound cs) = InfiniteSound $ f cs
mapComputation f (TimedSound d cs) = TimedSound d $ f cs

instance Show (Sound d Pulse) where
  show (TimedSound d c) = showSampledCompute d c
  show (InfiniteSound c) = showSampledCompute 3 c

showSampledCompute :: Duration -> ComputeSound Pulse -> String
showSampledCompute d cs = unsafePerformIO $ do
  let si = makeSamplingInfo (coerce $ 25 / d) d
  floats <- sampleComputeSound si cs
  pure $ show $ M.toList floats

instance Semigroup (Sound d Pulse) where
  -- \| Combines two sounds in a parallel manner (see 'parallel2')
  (<>) = parallel2

instance Monoid (Sound I Pulse) where
  mempty = silence

instance Monoid (Sound T Pulse) where
  mempty = TimedSound 0 $ makeDelayedResult $ const $ const 0

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
timedSequentially :: Sound T Pulse -> Sound T Pulse -> Sound T Pulse
timedSequentially (TimedSound d1 c1) (TimedSound d2 c2) =
  TimedSound (d1 + d2) $
    computeSequentially (coerce $ d1 / (d1 + d2)) c1 c2

infiniteSequentially :: Percentage -> Sound I Pulse -> Sound I Pulse -> Sound I Pulse
infiniteSequentially factor' (InfiniteSound c1) (InfiniteSound c2) =
  InfiniteSound $
    computeSequentially factor c1 c2
  where
    factor = max 0 $ min 1 factor'

-- | Same as 'sequentially2'
(>>>) :: Sound T Pulse -> Sound T Pulse -> Sound T Pulse
(>>>) = timedSequentially

infixl 5 >>>

-- | Combine a list of sounds in a sequential manner.
sequentially :: [Sound T Pulse] -> Sound T Pulse
sequentially = foldl' timedSequentially mempty

-- | Get the time for each sample which can be used for sinus wave calculations (e.g. 'pulse')
time :: Sound I Time
time = InfiniteSound $ do
  makeDelayedResult $ \si index ->
    coerce $ fromIntegral index * si.period

-- | Get the 'Progress' of a 'Sound'.
-- 'Progress' of '0' means that the sound has just started
-- 'Progress' of '1' means that the sound has finished
-- 'Progress' greater than '1' or smaller than '0' is invalid
progress :: Sound I Progress
progress = InfiniteSound $ makeDelayedResult $ \si index ->
  fromIntegral index / fromIntegral si.samples

-- | Tells you the sample index for each sample
sampleIndex :: Sound I Int
sampleIndex = InfiniteSound $ makeDelayedResult (const id)

-- | Combine two sounds such that they play in parallel. If one 'Sound' is longer than the other,
-- it will be played without the shorter one for its remaining time
parallel2 :: Sound d Pulse -> Sound d Pulse -> Sound d Pulse
parallel2 (InfiniteSound c1) (InfiniteSound c2) = InfiniteSound $ computeParallel c1 1 c2
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
constant a = InfiniteSound $ makeDelayedResult $ const (const a)

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

-- | Zip two 'Sound's. The duration of the resulting 'Sound' is equivalent
-- to the duration of the shorter 'Sound', cutting away the excess samples from the longer one.
zipSound :: Sound d1 (a -> b) -> Sound d2 a -> Sound (DetermineDuration d1 d2) b
zipSound = zipSoundWith ($)

-- | Amplifies the volume of the given 'Sound'
amplify :: Float -> Sound d Pulse -> Sound d Pulse
amplify x = fmap (* coerce x)

-- | Reduces the volume of the given 'Sound'
reduce :: Float -> Sound d Pulse -> Sound d Pulse
reduce x = amplify (1 / x)

-- | Raises the frequency of the 'Sound' by the given factor.
-- Only works if the sound is based on some frequency (e.g. 'pulse' but not 'noise')
raise :: Float -> Sound d Pulse -> Sound d Pulse
raise x = mapComputation $ \(ComputeSound compute) -> ComputeSound $ \si memo -> do
  compute (si {period = coerce x * si.period}) memo

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

-- | Drop the duration associated with a 'Sound' and get an infinite sound again
-- If you have combined timed sounds with a sequence combinator and then drop
-- their duration, the sounds will keep their proportional length to each other.
-- The percentage of their play time stays the same.
dropDuration :: Sound d a -> Sound I a
dropDuration (InfiniteSound cs) = InfiniteSound cs
dropDuration (TimedSound _ cs) = InfiniteSound cs

-- | Scales the 'Duration' of a 'Sound'.
-- The following makes a sound twice as long:
-- > scaleDuration 2 sound
scaleDuration :: Float -> Sound T a -> Sound T a
scaleDuration x (TimedSound d c) = TimedSound (coerce x * d) c

-- | Get the duration of a 'T'imed 'Sound'
getDuration :: Sound T a -> Duration
getDuration (TimedSound d _) = d

-- | Set the 'Duration' of a 'Sound' to the same as another one 'Sound'
adoptDuration :: Sound d a -> Sound x b -> Sound d b
adoptDuration (TimedSound duration _) = setDuration duration
adoptDuration (InfiniteSound _) = dropDuration

-- | Reverses a 'Sound' similar to 'reverse' for lists
reverseSound :: Sound d a -> Sound d a
reverseSound = mapComputation $ mapDelayedResult $ \si ->
  MU.unsafeBackpermute (M.Sz1 si.samples) (\index -> pred si.samples - index)

-- | Drop parts of a sound similar to 'drop' for lists
dropSound :: Duration -> Sound T a -> Sound T a
dropSound dropD' (TimedSound originalD cs) =
  TimedSound (originalD - dropD) $
    withSamplingInfoCS $ \oldSI ->
      changeSamplingInfo (\si -> si {samples = round $ factor * fromIntegral si.samples}) $
        mapDelayedResult
          ( \newSI ->
              MU.unsafeBackpermute (M.Sz1 oldSI.samples) $ \index ->
                index + newSI.samples - oldSI.samples
          )
          cs
  where
    dropD = max 0 $ min originalD dropD'
    droppedFactor = min 1 $ dropD / originalD
    factor =
      if droppedFactor == 1
        then 0
        else 1 / (1 - droppedFactor)

-- | Take parts of a sound similar to 'take' for lists
takeSound :: Duration -> Sound T a -> Sound T a
takeSound takeD' (TimedSound originalD cs) =
  TimedSound takeD $
    withSamplingInfoCS $ \oldSI ->
      changeSamplingInfo
        ( \si ->
            si
              { samples =
                  if takeD == 0
                    then 0
                    else round $ fromIntegral @_ @Float si.samples * (1 / coerce factor)
              }
        )
        $ mapDelayedResult
          (\_ -> M.slice' 0 $ M.Sz1 oldSI.samples)
          cs
  where
    takeD = max 0 $ min takeD' originalD
    factor = takeD / originalD

-- | Change how the 'Sound' progresses. For example, you can slow it
-- down in the beginning and speed it up at the end. However, the total
-- duration stays the same.
--
-- Negative 'Progress' is treated as '0' and 'Progress' above '1' is treated as '1'
changeTempo :: (Progress -> Progress) -> Sound d a -> Sound d a
changeTempo f = mapComputation $ mapDelayedResult $ \si ->
  MU.unsafeBackpermute (M.Sz1 si.samples) $ \index ->
    min si.samples $
      round $
        f
          (fromIntegral index / fromIntegral si.samples)
          * fromIntegral si.samples

changeTempoM :: Sound I (Progress -> Progress) -> Sound d a -> Sound d a
changeTempoM (InfiniteSound msc1) =
  mapComputation $
    mergeDelayedResult
      ( \si progressVector valueVector ->
          M.makeArray M.Seq (M.Sz1 si.samples) $ \index ->
            MU.unsafeIndex valueVector $
              min si.samples $
                round $
                  MU.unsafeIndex
                    progressVector
                    index
                    (fromIntegral index / fromIntegral si.samples)
                    * fromIntegral si.samples
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
convolve (Kernel coefficients sizeP offsetP) = mapComputation $ mapSoundFromMemory $ \wholeSound ->
  let n = M.unSz $ M.size wholeSound
      size = ceiling $ sizeP * fromIntegral n
      offset = round $ offsetP * fromIntegral n
      stencil = M.makeStencil (M.Sz1 size) offset $ \getV ->
        M.sum $ M.imap (\i -> (*) $ getV (i - offset)) computedCoefficients
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
computeOnce f = mapComputation $ mapDelayedResult $ \si ->
  let a = f si
   in M.map ($ a)

-- | Fill a sound with a vector of sound samples. Keep in mind that the vector has the appropriate length! 
fillWholeSound :: M.Load r M.Ix1 Pulse => (SamplingInfo -> M.Vector r Pulse) -> Sound I Pulse
fillWholeSound f = InfiniteSound $ fillSoundInMemoryIO $ \si dest -> do
  let vector = f si
  M.computeInto dest vector

-- | Fill a sound with a vector of sound samples in a mutable fashion.
fillWholeSoundST :: (SamplingInfo -> M.MVector M.RealWorld M.S Pulse -> ST M.RealWorld ()) -> Sound I Pulse
fillWholeSoundST f = InfiniteSound $ fillSoundInMemoryIO $ fmap stToIO . f

-- | Modify all samples of a sound so that you can look into the past and future
-- of a sound (e.g. IIR filter).
modifyWholeSound :: (M.Load r M.Ix1 Pulse, M.Size r) => (M.Vector M.S Pulse -> M.Vector r Pulse) -> Sound d Pulse -> Sound d Pulse
modifyWholeSound f = mapComputation $ mapSoundFromMemory f

-- | Modify all samples of a sound so that you can look into the past and future
-- of a sound (e.g. IIR filter).
modifyWholeSoundST :: (M.Vector M.S Pulse -> M.MVector M.RealWorld M.S Pulse -> ST M.RealWorld ()) -> Sound d Pulse -> Sound d Pulse
modifyWholeSoundST f = mapComputation $ mapSoundFromMemoryIO $ fmap stToIO . f

-- | Access the sample rate of an infinite sound
withSamplingInfo :: (SamplingInfo -> Sound d a) -> Sound I a
withSamplingInfo f = InfiniteSound $ withSamplingInfoCS (getCS . f)
