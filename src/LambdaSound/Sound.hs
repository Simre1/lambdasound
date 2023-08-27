module LambdaSound.Sound where

import Data.Coerce
import Data.Foldable (foldl')
import Data.Maybe (fromMaybe)
import Data.Vector qualified as V

newtype Pulse = Pulse Float deriving (Show, Eq, Floating, Num, Fractional, Ord, Real, RealFrac)

newtype Duration = Duration Float deriving (Show, Eq, Floating, Num, Fractional, Ord, Real, RealFrac)

newtype Progress = Progress Float deriving (Show, Eq, Floating, Num, Fractional, Ord, Real, RealFrac)

newtype Percentage = Percentage Float deriving (Show, Eq, Floating, Num, Fractional, Ord, Real, RealFrac)

data SampleRate = SampleRate
  { period :: !Float,
    samples :: !Int
  }

data CurrentSample = CurrentSample
  { index :: !Int,
    progress :: !Progress
  }

csFromSr :: SampleRate -> Int -> CurrentSample
csFromSr sr i = CurrentSample i (fromIntegral i / fromIntegral (sr.samples - 1))

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

expand :: Sound T a -> Sound T (V.Vector a)
expand (TimedSound d c) = TimedSound d $ \sr ->
  let expanded = V.generate (sr.samples) $ \index ->
        c sr $ csFromSr sr index
   in const expanded
{-# INLINE expand #-}

sequentially2 :: Sound T a -> Sound T a -> Sound T a
sequentially2 (TimedSound d1 c1) (TimedSound d2 c2) = TimedSound (d1 + d2) $ \sr ->
  let factor = coerce $ d1 / (d1 + d2)
      splitIndex =
        round $ factor * fromIntegral sr.samples
      f1 = c1 $ sr {samples = splitIndex}
      f2 = c2 $ sr {samples = sr.samples - splitIndex}
   in \cr ->
        if cr.progress < factor
          then f1 $ cr {progress = cr.progress * (1 / factor)}
          else f2 $ cr {index = cr.index - splitIndex, progress = (cr.progress - factor) * (1 / (1 - factor))}
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
progress = InfiniteSound $ \_ cs -> cs.progress
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
        then \cs -> f1 cs + if cs.progress < d2Percentage then f2 cs {progress = cs.progress / d2Percentage} else 0
        else \cs -> f2 cs + if cs.progress < d1Percentage then f1 cs {progress = cs.progress / d1Percentage} else 0
  where
    d1Percentage = coerce $ d1 / newDuration
    d2Percentage = coerce $ d2 / newDuration
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
   in \cs -> c' cs {progress = 1 - cs.progress}

dropSound :: Duration -> Sound T a -> Sound T a
dropSound dropD' (TimedSound originalD c) = TimedSound (originalD - dropD) $ \sr ->
  let c' = c sr {samples = round $ fromIntegral @_ @Float sr.samples * (1 / coerce factor)}
   in \cs -> c' cs {progress = coerce droppedFactor + cs.progress * coerce factor}
  where
    dropD = max 0 $ min originalD dropD'
    droppedFactor = dropD / originalD
    factor = 1 - droppedFactor

takeSound :: Duration -> Sound T a -> Sound T a
takeSound takeD' (TimedSound originalD c) = TimedSound takeD $ \sr ->
  let c' = c sr {samples = round $ fromIntegral @_ @Float sr.samples * (1 / coerce factor)}
   in \cs -> c' cs {progress = cs.progress * coerce factor}
  where
    takeD = max 0 $ min takeD' originalD
    factor = takeD / originalD

changeTempo :: (Progress -> Progress) -> Sound d a -> Sound d a
changeTempo f = mapComputation $ \c sr ->
  let c' = c sr
   in \cs -> c' cs {progress = f $ cs.progress}

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
  let wholeSound = V.generate (sr.samples) $ \index ->
        c sr $ csFromSr sr index
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
