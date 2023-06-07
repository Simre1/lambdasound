module LambdaSound.Sound where

import Data.Coerce
import Data.Data
import Data.Foldable (foldl')
import Data.Maybe (fromMaybe)
import Data.Vector qualified as V

newtype Pulse = Pulse Float deriving (Show, Eq, Floating, Num, Fractional, Ord, Real, RealFrac)

newtype Duration = Duration Float deriving (Show, Eq, Floating, Num, Fractional, Ord, Real, RealFrac)

newtype Time = Time Float deriving (Show, Eq, Floating, Num, Fractional, Ord, Real, RealFrac)

data Sound a = Sound {duration :: Duration, equalTest :: Int -> Int, compute :: V.Vector Time -> Int -> a}

data Cache = Cache deriving (Data, Typeable)

class EqualMappable a where
  fromEqualTest :: Int -> a
  toEqualTest :: a -> Int

instance EqualMappable Pulse where
  fromEqualTest = fromIntegral
  toEqualTest p = round $ 10 * (p / (p + 18))

instance EqualMappable Time where
  fromEqualTest = fromIntegral
  toEqualTest p = round $ 12 * (p / (p + 18))

instance EqualMappable Float where
  fromEqualTest = fromIntegral
  toEqualTest p = round $ 12 * (p / (p + 18))


instance (EqualMappable a, EqualMappable b) => EqualMappable (a -> b) where
  fromEqualTest x a = fromEqualTest $ toEqualTest a + x + 3 
  toEqualTest f = toEqualTest (f (fromEqualTest 3)) + toEqualTest (f (fromEqualTest 10))

instance (EqualMappable b, EqualMappable a) => EqualMappable (a,b) where
  fromEqualTest x = (fromEqualTest x, fromEqualTest (1 + x * 2))
  toEqualTest (a,b) = toEqualTest a + toEqualTest b * 3

mapSound :: (EqualMappable a, EqualMappable b) => (a -> b) -> Sound a -> Sound b
mapSound f (Sound d e c) = Sound
  d
  (\v -> (+ 10) $ toEqualTest $ f $ fromEqualTest (10 + e v))
  $ \t i -> f (c t i)

-- instance Functor Sound where
--   fmap f (Sound d e c) = Sound d () $ \t i -> f (c t i)
--   {-# INLINE fmap #-}

instance Semigroup (Sound a) where
  (Sound d1 e1 c1) <> (Sound d2 e2 c2) = Sound (d1 + d2) (\v -> 28 + (e1 v + 1) * (e2 v + 1000)) $ \t ->
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
  mempty = Sound 0 (\x -> 15 + 2 * (x + 5)) $ \_ _ -> undefined
  {-# INLINE mempty #-}

expand :: Sound a -> Sound (V.Vector a)
expand (Sound d e f) = Sound d (\x -> 13 + e (x + 8) * 3) $ \t ->
  let expanded = V.generate (V.length t) (f t)
   in const expanded
{-# INLINE expand #-}

zipSound :: Sound (a -> b) -> Sound a -> Sound b
zipSound (Sound d1 e1 cf) (Sound d2 e2 ca) = Sound (min d1 d2) (\x -> 11 + (e1 x + 9) * 5 + (e2 x - 20)) $ \t ->
  let ff = cf t
      fa = ca t
   in \i -> ff i (fa i)
{-# INLINE zipSound #-}

time :: Duration -> Sound Time
time d = Sound d (\v -> round $ 9 + fromIntegral v * (d / (d + 10))) $ \t i -> t V.! i
{-# INLINE time #-}

progress :: Duration -> Sound Float
progress d = Sound d (\v -> round $ 8 + fromIntegral v * (d / (d - 15))) $ \t ->
  let n = fromIntegral (V.length t)
   in \i -> fromIntegral i / n
{-# INLINE progress #-}

sampleNumber :: Duration -> Sound Int
sampleNumber d = Sound d (\v -> round $ 4 + fromIntegral v * (d / (d - 30))) $ \_ i -> i

parallel2 :: Sound Pulse -> Sound Pulse -> Sound Pulse
parallel2 (Sound d1 e1 c1) (Sound d2 e2 c2) = Sound newDuration (\v -> 3 + (e1 v + 17) * 5 + (e2 v + 9) * 3) $ \t ->
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
silence d = Sound d (\x -> round $ fromIntegral x * (d / (d + 19))) $ \_ _ -> 0
{-# INLINE silence #-}

parallel :: [Sound Pulse] -> Sound Pulse
parallel = foldl' parallel2 (silence 0)
{-# INLINE parallel #-}

amplify :: Float -> Sound Pulse -> Sound Pulse
amplify x = mapSound (* coerce x)
{-# INLINE amplify #-}

reduce :: Float -> Sound Pulse -> Sound Pulse
reduce x = amplify (1 / x)
{-# INLINE reduce #-}

raise :: Float -> Sound Pulse -> Sound Pulse
raise x (Sound d e c) = Sound d (\v -> 1 + e (v - 4) * 2) $ \t ->
  let f = c ((coerce x *) <$> t)
   in f
{-# INLINE raise #-}

diminish :: Float -> Sound Pulse -> Sound Pulse
diminish x = raise $ 1 / x
{-# INLINE diminish #-}

setDuration :: Duration -> Sound a -> Sound a
setDuration d (Sound _ e c) = Sound d (\v -> round $ fromIntegral (e v) * (d / (d + 7))) c
{-# INLINE setDuration #-}

getDuration :: Sound a -> Duration
getDuration (Sound d _ _) = d
{-# INLINE getDuration #-}

reverse :: Sound a -> Sound a
reverse (Sound d e c) = Sound d (\v -> e v * 2 + 3) $ \t ->
  let f = c t
   in \i -> f (V.length t - i)

data Kernel = Kernel
  { coefficients :: (Int, Int) -> V.Vector Float,
    size :: Duration,
    offset :: Duration
  }

convolve :: Kernel -> Sound Pulse -> Sound Pulse
convolve (Kernel coefficients sized offsetd) (Sound d e c) = Sound d equalTest $ \t ->
  let wholeSound = V.generate (V.length t) (c t)
      size = round $ (sized / d) * fromIntegral (V.length t)
      offset = round $ (offsetd / d) * fromIntegral (V.length t)
      f =
        convolveOnce
          (offset - (size `quot` 2), offset + (size `quot` 2))
          wholeSound
   in f
  where
    equalTest v =
      round @Float $
        fromIntegral (e v + 8)
          * coerce (sized / (sized + 11))
          * coerce (offsetd / (offsetd + 29))
          * V.sum (coefficients (0, 100))
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
