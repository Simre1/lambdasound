module LambdaSound.Sound
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
    zipSound,
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
    evaluate,

    -- ** Convolution
    Kernel (..),
    convolve,
    convolveDuration,

    -- ** Making new sounds
    time,
    progress,
    sampleIndex,
    computeOnce,
    computeWholeSound,
    constant,
    silence,
  )
where

import Control.DeepSeq (NFData)
import Data.Coerce
import Data.Foldable (foldl')
import Data.Functor ((<&>))
import Data.Massiv.Array qualified as M

import Foreign.Storable (Storable)

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

-- | A 'Sound' represent a computation to generate audio samples
data Sound (d :: SoundDuration) a where
  TimedSound ::
    !Duration ->
    (SampleRate -> CurrentSample -> a) ->
    Sound T a
  InfiniteSound ::
    (SampleRate -> CurrentSample -> a) ->
    Sound I a

instance (Show a) => Show (Sound d a) where
  show (TimedSound d c) = showSampledCompute d c
  show (InfiniteSound c) = showSampledCompute 3 c

showSampledCompute :: (Show a) => Duration -> (SampleRate -> CurrentSample -> a) -> String
showSampledCompute d c =
  let c' = c sr
      sr = SampleRate (1 / 25) (ceiling $ d * 25)
   in show $ M.generate M.Seq (M.Sz1 $ ceiling $ d * 25) $ c' . CurrentSample

instance Functor (Sound d) where
  fmap f (TimedSound d c) = TimedSound d $ \sr -> f . c sr
  fmap f (InfiniteSound c) = InfiniteSound $ \sr -> f . c sr
  {-# INLINE fmap #-}

instance Semigroup (Sound d Pulse) where
  -- \| Combines two sounds in a parallel manner (see 'parallel2')
  (<>) = parallel2
  {-# INLINE (<>) #-}

instance Monoid (Sound I Pulse) where
  mempty = silence
  {-# INLINE mempty #-}

instance Monoid (Sound T Pulse) where
  mempty = TimedSound 0 $ \_ _ -> 0
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

-- | Compute a value once and then reuse it while computing all samples
computeOnce :: (SampleRate -> a) -> Sound d (a -> b) -> Sound d b
computeOnce f = mapComputation $ \c sr ->
  let a = f sr
      c' = c sr
   in flip c' a
{-# INLINE computeOnce #-}

-- | Compute a whole sound so that you can look into the past and future
-- of a sound (e.g. IIR filter).
computeWholeSound :: (M.Vector M.D a -> CurrentSample -> b) -> Sound d a -> Sound d b
computeWholeSound f = mapComputation $ \c sr ->
  let c' = c sr
      a = M.generate M.Seq (M.Sz1 sr.samples) $ c' . CurrentSample
   in f a

-- | Append two sounds. This is only possible for sounds with a duration.
sequentially2 :: Sound T a -> Sound T a -> Sound T a
sequentially2 (TimedSound d1 c1) (TimedSound d2 c2) = TimedSound (d1 + d2) $ \sr ->
  let factor = d1 / (d1 + d2)
      splitIndex =
        round $ factor * fromIntegral sr.samples
      f1 = c1 $ sr {samples = splitIndex}
      f2 = c2 $ sr {samples = sr.samples - splitIndex}
   in \cr ->
        if cr.index < splitIndex
          then f1 cr
          else f2 $ cr {index = cr.index - splitIndex}
{-# INLINE sequentially2 #-}

-- | Same as 'sequentially2'
(>>>) :: Sound T a -> Sound T a -> Sound T a
(>>>) = sequentially2

-- | Combine a list of sounds in a sequential manner.
sequentially :: [Sound T Pulse] -> Sound T Pulse
sequentially = foldl' sequentially2 mempty
{-# INLINE sequentially #-}

-- | Get the time for each sample which can be used for sinus wave calculations (e.g. 'pulse')
time :: Sound I Float
time = InfiniteSound $ \sr cs ->
  fromIntegral cs.index * sr.period
{-# INLINE time #-}

-- | Get the 'Progress' of a 'Sound'.
-- 'Progress' of '0' means that the sound has just started
-- 'Progress' of '1' means that the sound has finished
-- 'Progress' greater than '1' or smaller than '0' is invalid
progress :: Sound I Progress
progress = InfiniteSound $ \sr cs -> fromIntegral cs.index / fromIntegral sr.samples
{-# INLINE progress #-}

-- | Tells you the sample index for each sample
sampleIndex :: Sound I Int
sampleIndex = InfiniteSound $ \_ cs -> cs.index
{-# INLINE sampleIndex #-}

-- | Combine two sounds such that they play in parallel
parallel2 :: Sound d Pulse -> Sound d Pulse -> Sound d Pulse
parallel2 (TimedSound d1 c1) (TimedSound d2 c2) = TimedSound newDuration $ \sr ->
  let d1N = round $ d1Percentage * fromIntegral sr.samples
      d2N = round $ d2Percentage * fromIntegral sr.samples
      f1 = c1 sr {samples = d1N}
      f2 = c2 sr {samples = d2N}
   in if d1 >= d2
        then \cs -> f1 cs + if cs.index < d2N then f2 cs else 0
        else \cs -> f2 cs + if cs.index < d1N then f1 cs else 0
  where
    d1Percentage = d1 / newDuration
    d2Percentage = d2 / newDuration
    newDuration = max d1 d2
parallel2 (InfiniteSound c1) (InfiniteSound c2) = InfiniteSound $ \sr ->
  let c1' = c1 sr
      c2' = c2 sr
   in \cs -> c1' cs + c2' cs
{-# INLINE parallel2 #-}

-- | Combine a lists of sounds such that they play in parallel
parallel :: (Monoid (Sound d Pulse)) => [Sound d Pulse] -> Sound d Pulse
parallel = foldl' parallel2 mempty
{-# INLINE parallel #-}

-- | A 'Sound' with '0' volume
silence :: Sound I Pulse
silence = InfiniteSound $ \_ _ -> 0
{-# INLINE silence #-}

-- | A constant 'Sound'
constant :: a -> Sound I a
constant a = InfiniteSound $ \_ _ -> a

-- | Zip two 'Sound's. The duration of the resulting 'Sound' is equivalent
-- to the duration of the shorter 'Sound', cutting away the excess samples from the longer one.
zipSound :: Sound d1 (a -> b) -> Sound d2 a -> Sound (DetermineDuration d1 d2) b
zipSound sound1 sound2 =
  case (sound1, sound2) of
    (TimedSound d1 _, TimedSound d2 _) ->
      let d = min d1 d2
       in case (takeSound d sound1, takeSound d sound2) of
            (TimedSound _ c1, TimedSound _ c2) -> TimedSound d (c c1 c2)
    (TimedSound d c1, InfiniteSound c2) -> TimedSound d (c c1 c2)
    (InfiniteSound c1, TimedSound d c2) -> TimedSound d (c c1 c2)
    (InfiniteSound c1, InfiniteSound c2) -> InfiniteSound (c c1 c2)
  where
    c cf ca sr =
      let ff = cf sr
          fa = ca sr
       in \cs -> ff cs (fa cs)
{-# INLINE zipSound #-}

-- | Similar to 'zipSound'
zipSoundWith :: (a -> b -> c) -> Sound d1 a -> Sound d2 b -> Sound (DetermineDuration d1 d2) c
zipSoundWith f s1 = zipSound (f <$> s1)
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
raise x (TimedSound d c) = TimedSound d $ \sr ->
  c $ sr {period = coerce x * sr.period}
raise x (InfiniteSound c) = InfiniteSound $ \sr ->
  c $ sr {period = coerce x * sr.period}
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
reverseSound = mapComputation $ \c sr ->
  let c' = c sr
   in \cs -> c' cs {index = pred sr.samples - cs.index}
{-# INLINE reverseSound #-}

-- | Drop parts of a sound similar to 'drop' for lists
dropSound :: Duration -> Sound T a -> Sound T a
dropSound dropD' (TimedSound originalD c) = TimedSound (originalD - dropD) $ \sr ->
  let c' = c sr {samples = paddedSamples sr}
   in \cs ->
        c' $
          let !x = cs {index = cs.index + paddedSamples sr - sr.samples}
           in x
  where
    dropD = max 0 $ min originalD dropD'
    droppedFactor = dropD / originalD
    factor = 1 - droppedFactor
    paddedSamples sr = round $ fromIntegral @_ @Float sr.samples * (1 / coerce factor)
{-# INLINE dropSound #-}

-- | Take parts of a sound similar to 'take' for lists
takeSound :: Duration -> Sound T a -> Sound T a
takeSound takeD' (TimedSound originalD c) = TimedSound takeD $ \sr ->
  let c' = c sr {samples = round $ fromIntegral @_ @Float sr.samples * (1 / coerce factor)}
   in c'
  where
    takeD = max 0 $ min takeD' originalD
    factor = takeD / originalD
{-# INLINE takeSound #-}

-- | Change how the 'Sound' progresses. For example, you can slow it
-- down in the beginning and speed it up at the end. However, the total
-- duration stays the same.
--
-- Negative 'Progress' is treated as '0' and 'Progress' above '1' is treated as '1'
changeTempo :: (Progress -> Progress) -> Sound d a -> Sound d a
changeTempo f = mapComputation $ \c sr ->
  let c' = c sr
   in c' . changeIndex sr
  where
    changeIndex sr cs =
      cs
        { index =
            min sr.samples $
              round $
                f
                  (fromIntegral cs.index / fromIntegral sr.samples)
                  * fromIntegral sr.samples
        }
{-# INLINE changeTempo #-}

mapComputation :: ((SampleRate -> CurrentSample -> a) -> SampleRate -> CurrentSample -> b) -> Sound d a -> Sound d b
mapComputation f (InfiniteSound c) = InfiniteSound $ f c
mapComputation f (TimedSound d c) = TimedSound d $ f c

-- | A Kernel for convolution
data Kernel p = Kernel
  { coefficients :: Percentage -> Float,
    size :: p,
    offset :: p
  }

evaluate :: Sound d Pulse -> Sound d Pulse
evaluate = mapComputation $ \c sr ->
  let !wholeSound = M.compute @M.S $ M.generate M.Seq (M.Sz1 sr.samples) $ c' . coerce
      c' = c sr
  in \cs -> wholeSound M.! cs.index
-- | Convolvution of a 'Sound' where the 'Kernel' size is
-- determined by 'Percentage's of the sound
convolve :: Kernel Percentage -> Sound d Pulse -> Sound d Pulse
convolve (Kernel coefficients sizeP offsetP) = mapComputation $ \c sr ->
  let size = ceiling $ sizeP * fromIntegral sr.samples
      offset = round $ offsetP * fromIntegral sr.samples
      
      c' = c sr
      wholeSound = M.compute @M.S $ M.generate M.Seq (M.Sz1 sr.samples) $ c' . CurrentSample
      
      stencil = M.makeStencil (M.Sz1 size) offset $ \get ->
        sum $ [0 .. pred size] <&> \i -> get (i - offset) * (computedCoefficients M.! i)
      computedCoefficients =
        M.compute @M.S $
          if size <= 1
            then M.singleton 0.5
            else M.generate M.Seq (M.Sz1 size) $ \i ->
              coerce @_ @Pulse $
                coefficients (fromIntegral i / fromIntegral (size - 1))

      convolved = M.compute @M.S $ M.mapStencil M.Reflect stencil wholeSound
   in \cs -> convolved M.! cs.index
{-# INLINE convolve #-}

-- | Convolution of a 'Sound' where the 'Kernel' size is
-- determined by a 'Duration'.
convolveDuration :: Kernel Duration -> Sound T Pulse -> Sound T Pulse
convolveDuration (Kernel coefficients sizeD offsetD) sound@(TimedSound d _) =
  convolve
    (Kernel coefficients (coerce $ sizeD / d) (coerce $ offsetD / d))
    sound
{-# INLINE convolveDuration #-}
