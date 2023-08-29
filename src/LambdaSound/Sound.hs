module LambdaSound.Sound where

import Data.Coerce
import Data.Foldable (foldl')
import Data.Maybe (fromMaybe)
import Data.Vector qualified as V
import Debug.Trace

newtype Pulse = Pulse Float deriving (Show, Eq, Floating, Num, Fractional, Ord, Real, RealFrac)

newtype Duration = Duration Float deriving (Show, Eq, Floating, Num, Fractional, Ord, Real, RealFrac)

newtype Progress = Progress Float deriving (Show, Eq, Floating, Num, Fractional, Ord, Real, RealFrac)

newtype Percentage = Percentage Float deriving (Show, Eq, Floating, Num, Fractional, Ord, Real, RealFrac)

data SampleRate = SampleRate
  { period :: !Float,
    samples :: !Int
  }
  deriving (Show)

newtype CurrentSample = CurrentSample
  { index :: Int
  }
  deriving (Show)

data SoundDuration = I | T

type family DetermineDuration (d1 :: SoundDuration) (d2 :: SoundDuration) where
  DetermineDuration I d = d
  DetermineDuration d I = d
  DetermineDuration T _ = T
  DetermineDuration _ T = T

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
   in show $ V.generate (ceiling $ d * 25) $ c' . CurrentSample

instance Functor (Sound d) where
  fmap f (TimedSound d c) = TimedSound d $ \sr -> f . c sr
  fmap f (InfiniteSound c) = InfiniteSound $ \sr -> f . c sr
  {-# INLINE fmap #-}

instance Semigroup (Sound d Pulse) where
  (<>) = parallel2
  {-# INLINE (<>) #-}

instance Monoid (Sound I Pulse) where
  mempty = silence
  {-# INLINE mempty #-}

instance Monoid (Sound T Pulse) where
  mempty = TimedSound 0 $ \_ _ -> 0
  {-# INLINE mempty #-}

computeOnce :: (SampleRate -> a) -> Sound d (a -> b) -> Sound d b
computeOnce f = mapComputation $ \c sr ->
  let a = f sr
      c' = c sr
   in \cs -> c' cs a
{-# INLINE computeOnce #-}

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

(>>>) :: Sound T a -> Sound T a -> Sound T a
(>>>) = sequentially2

sequentially :: [Sound T Pulse] -> Sound T Pulse
sequentially = foldl' sequentially2 mempty
{-# INLINE sequentially #-}

time :: Sound I Float
time = InfiniteSound $ \sr cs ->
  fromIntegral cs.index * sr.period
{-# INLINE time #-}

progress :: Sound I Progress
progress = InfiniteSound $ \sr cs -> fromIntegral cs.index / fromIntegral sr.samples
{-# INLINE progress #-}

sampleIndex :: Sound I Int
sampleIndex = InfiniteSound $ \_ cs -> cs.index
{-# INLINE sampleIndex #-}

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

parallel :: (Monoid (Sound d Pulse)) => [Sound d Pulse] -> Sound d Pulse
parallel = foldl' parallel2 mempty
{-# INLINE parallel #-}

silence :: Sound I Pulse
silence = InfiniteSound $ \_ _ -> 0
{-# INLINE silence #-}

constant :: a -> Sound I a
constant a = InfiniteSound $ \_ _ -> a

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

amplify :: Float -> Sound d Pulse -> Sound d Pulse
amplify x = fmap (* coerce x)
{-# INLINE amplify #-}

reduce :: Float -> Sound d Pulse -> Sound d Pulse
reduce x = amplify (1 / x)
{-# INLINE reduce #-}

raise :: Float -> Sound d Pulse -> Sound d Pulse
raise x (TimedSound d c) = TimedSound d $ \sr ->
  c $ sr {period = coerce x * sr.period}
raise x (InfiniteSound c) = InfiniteSound $ \sr ->
  c $ sr {period = coerce x * sr.period}
{-# INLINE raise #-}

diminish :: Float -> Sound d Pulse -> Sound d Pulse
diminish x = raise $ 1 / x
{-# INLINE diminish #-}

setDuration :: Duration -> Sound d a -> Sound T a
setDuration d (TimedSound _ c) = TimedSound d c
setDuration d (InfiniteSound c) = TimedSound d c
{-# INLINE setDuration #-}

getDuration :: Sound T a -> Duration
getDuration (TimedSound d _) = d
{-# INLINE getDuration #-}

reverseSound :: Sound d a -> Sound d a
reverseSound = mapComputation $ \c sr ->
  let c' = c sr
   in \cs -> c' cs {index = pred sr.samples - cs.index}

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

takeSound :: Duration -> Sound T a -> Sound T a
takeSound takeD' (TimedSound originalD c) = TimedSound takeD $ \sr ->
  let c' = c sr {samples = round $ fromIntegral @_ @Float sr.samples * (1 / coerce factor)}
   in c'
  where
    takeD = max 0 $ min takeD' originalD
    factor = takeD / originalD

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

mapComputation :: ((SampleRate -> CurrentSample -> a) -> SampleRate -> CurrentSample -> b) -> Sound d a -> Sound d b
mapComputation f (InfiniteSound c) = InfiniteSound $ f c
mapComputation f (TimedSound d c) = TimedSound d $ f c

data Kernel p = Kernel
  { coefficients :: Percentage -> Float,
    size :: p,
    offset :: p
  }

convolve :: Kernel Percentage -> Sound d Pulse -> Sound d Pulse
convolve (Kernel coefficients sizeP offsetP) = mapComputation $ \c sr ->
  let wholeSound = V.generate (sr.samples) $ c sr . CurrentSample
      size = ceiling $ sizeP * fromIntegral sr.samples
      offset = round $ offsetP * fromIntegral sr.samples
      f = convolveOnce size offset wholeSound
   in \cs -> f cs.index
  where
    convolveOnce :: Int -> Int -> V.Vector Pulse -> Int -> Pulse
    convolveOnce size offset vector =
      let computedCoefficients =
            if size <= 1
              then V.singleton 0.5
              else V.generate size $ \i -> coefficients (fromIntegral i / fromIntegral (size - 1))
          get i = fromMaybe 0 $ vector V.!? i
          indices = [0 .. size - 1]
       in \i ->
            sum $
              (\x -> get (i + x + offset) * coerce (computedCoefficients V.! x))
                <$> indices

convolveDuration :: Kernel Duration -> Sound T Pulse -> Sound T Pulse
convolveDuration (Kernel coefficients sizeD offsetD) sound@(TimedSound d _) =
  convolve
    (Kernel coefficients (coerce $ sizeD / d) (coerce $ offsetD / d))
    sound
