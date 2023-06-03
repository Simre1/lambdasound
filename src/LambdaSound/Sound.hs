module LambdaSound.Sound where

import Data.Coerce
import Data.Foldable (foldl')
import Data.Maybe (fromMaybe)
import Data.Vector qualified as V
import Data.Data

newtype Pulse = Pulse Float deriving (Show, Eq, Floating, Num, Fractional, Ord, Real, RealFrac)

newtype Duration = Duration Float deriving (Show, Eq, Floating, Num, Fractional, Ord, Real, RealFrac)

newtype Time = Time Float deriving (Show, Eq, Floating, Num, Fractional, Ord, Real, RealFrac)

data Sound a = Sound {duration :: Duration, compute :: V.Vector Time -> Int -> a}

data Cache = Cache deriving (Data, Typeable)

instance Functor Sound where
  fmap f (Sound d c) = Sound d $ \t i -> f (c t i)
  {-# INLINE fmap #-}

instance Semigroup (Sound a) where
  (Sound d1 c1) <> (Sound d2 c2) = Sound (d1 + d2) $ \t ->
    let splitIndex =
          round $
            d1
              / (d1 + d2)
              * fromIntegral (length t)
        (t1, t2) = V.splitAt splitIndex t
        f1 = c1 t1
        f2 = c2 t2
     in \i ->
          if i < splitIndex
            then f1 i
            else f2 (i - splitIndex)
  {-# INLINE (<>) #-}

instance Monoid (Sound a) where
  mempty = Sound 0 $ \_ _ -> undefined
  {-# INLINE mempty #-}

expand :: Sound a -> Sound (V.Vector a)
expand (Sound d f) = Sound d $ \t ->
  let expanded = V.generate (V.length t) (f t)
   in const expanded
{-# INLINE expand #-}

zipSound :: Sound (a -> b) -> Sound a -> Sound b
zipSound (Sound d1 cf) (Sound d2 ca) = Sound (min d1 d2) $ \t ->
  let ff = cf t
      fa = ca t
   in \i -> ff i (fa i)
{-# INLINE zipSound #-}

time :: Duration -> Sound Time
time d = Sound d $ \t i -> t V.! i
{-# INLINE time #-}

progress :: Duration -> Sound Float
progress d = Sound d $ \t ->
  let n = fromIntegral (V.length t)
   in \i -> fromIntegral i / n
{-# INLINE progress #-}

sampleNumber :: Duration -> Sound Int
sampleNumber d = Sound d $ \_ i -> i

parallel2 :: Sound Pulse -> Sound Pulse -> Sound Pulse
parallel2 (Sound d1 c1) (Sound d2 c2) = Sound newDuration $ \t ->
  let d1N = round $ d1Percentage * fromIntegral (length t)
      d2N = round $ d2Percentage * fromIntegral (length t)
      f1 = c1 t
      f2 = c2 t
   in if d1N >= d2N
        then \i -> f1 i + if i < d2N then f2 i else 0
        else \i -> f2 i + if i < d1N then f1 i else 0
  where
    d1Percentage = d1 / newDuration
    d2Percentage = d2 / newDuration
    newDuration = max d1 d2
{-# INLINE parallel2 #-}

silence :: Duration -> Sound Pulse
silence d = Sound d $ \_ _ -> 0
{-# INLINE silence #-}

parallel :: [Sound Pulse] -> Sound Pulse
parallel = foldl' parallel2 (silence 0)
{-# INLINE parallel #-}

amplify :: Float -> Sound Pulse -> Sound Pulse
amplify x = fmap (* coerce x)
{-# INLINE amplify #-}

reduce :: Float -> Sound Pulse -> Sound Pulse
reduce x = amplify (1 / x)
{-# INLINE reduce #-}

raise :: Float -> Sound Pulse -> Sound Pulse
raise x (Sound d c) = Sound d $ \t ->
  let f = c ((coerce x *) <$> t)
   in f
{-# INLINE raise #-}

diminish :: Float -> Sound Pulse -> Sound Pulse
diminish x = raise $ 1 / x
{-# INLINE diminish #-}

setDuration :: Duration -> Sound a -> Sound a
setDuration d (Sound _ c) = Sound d c
{-# INLINE setDuration #-}

getDuration :: Sound a -> Duration
getDuration (Sound d _) = d
{-# INLINE getDuration #-}

reverse :: Sound a -> Sound a
reverse (Sound d c) = Sound d $ \t ->
  let f = c t
   in \i -> f (V.length t - i)

data Kernel = Kernel
  { coefficients :: (Int, Int) -> V.Vector Float,
    size :: Duration,
    offset :: Duration
  }

convolve :: Kernel -> Sound Pulse -> Sound Pulse
convolve (Kernel coefficients sized offsetd) (Sound d c) = Sound d $ \t ->
  let wholeSound = V.generate (V.length t) (c t)
      size = round $ (sized / d) * fromIntegral (V.length t)
      offset = round $ (offsetd / d) * fromIntegral (V.length t)
      f =
        convolveOnce
          (offset - (size `quot` 2), offset + (size `quot` 2))
          wholeSound
   in f
  where
    convolveOnce :: (Int, Int) -> V.Vector Pulse -> Int -> Pulse
    convolveOnce (start, end) vector =
      let computedCoefficients = coefficients (start, end)
          get i = fromMaybe 0 $ vector V.!? i
          indices = [0 .. (n - 1)]
          n = end - start
       in \i ->
            sum $
              (\x -> get (i + x) * coerce (computedCoefficients V.! x))
                <$> indices

-- dropSound :: (Monoid a) => Duration -> Sound a -> Sound a
-- dropSound dropD (Sound d c) = Sound (max 0 $ d - dropD) $ \t ->
--   let droppedIndices =
--         round $
--           dropD
--             / d
--             * fromIntegral (length t)
--       f = c t
--    in \i -> if i < length t - droppedIndices
--     then f (i - droppedIndices)
--     else mempty

-- takeSound :: (Monoid a) => Duration -> Sound a -> Sound a
-- takeSound takeD (Sound d c) = Sound takeD $ \t ->
--   let takenIndices =
--         round $
--           takeD
--             / d
--             * fromIntegral (length t)
--       f = c t
--    in \i -> if i < length t - droppedIndices
--     then f (i - droppedIndices)
--     else mempty
