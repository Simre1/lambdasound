module LambdaSound.Samples where

import Data.Coerce
import Data.Vector.Storable qualified as V
import LambdaSound.Sound
import System.Random as R

newtype Hz = Hz Float deriving (Show, Eq, Ord, Num, Fractional, Floating, Enum)

-- | Pure sinus sound
pulse :: Hz -> Sound I Pulse
pulse hz = (\t -> sin (coerce hz * coerce t * 2 * pi)) <$> time
{-# INLINE pulse #-}

-- | Sinus sound overlayed with some harmonic frequencies
harmonic :: Hz -> Sound I Pulse
harmonic hz = parallel $ (\x -> reduce x $ pulse (coerce x * hz)) <$> take 6 [1 ..]
{-# INLINE harmonic #-}

-- | Sinus sound overlayed with harmonic frequences higher pitched than 'harmonic'
harmonic2 :: Hz -> Sound I Pulse
harmonic2 hz = parallel $ (\x -> reduce (logBase 2 (x + 1)) $ pulse (coerce x * hz)) <$> take 6 [1 ..]
{-# INLINE harmonic2 #-}

-- | Random noise between (-1,1). The given value is used as the seed value,
-- so the same seed will result in the same noise
noise :: Int -> Sound I Pulse
noise initial =
  computeOnce
    ( \sr ->
        V.unfoldrExactN
          sr.samples
          (R.uniformR (-1, 1))
          (mkStdGen initial)
    )
    (fmap Pulse . flip (V.!) <$> sampleIndex)
{-# INLINE noise #-}
