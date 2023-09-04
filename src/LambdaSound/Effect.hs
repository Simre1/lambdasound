module LambdaSound.Effect where

import Data.Coerce
import LambdaSound.Sound

-- | Eases the volume of the sound. The given 'Int' controls the strength of the easing.
easeInOut :: Int -> Sound d Pulse -> Sound d Pulse
easeInOut strength = zipSoundWith (\p -> (f p *)) progress
  where
    f p = coerce $ -(2 * p - 1) ** (abs (fromIntegral strength) * 2) + 1
{-# INLINE easeInOut #-}

-- | Repeats a sound such that:
-- @
-- repeatSound 3 sound = sound >>> sound >>> sound
-- @
repeatSound :: Int -> Sound T Pulse -> Sound T Pulse
repeatSound n s
  | n <= 0 = mempty
  | n == 1 = s
  | even n = s' >>> s'
  | otherwise = s' >>> s' >>> s
  where
    s' = repeatSound (n `quot` 2) s
{-# INLINE repeatSound #-}

-- | Reverbs a sound for the specified duration. This is not working well right now.
reverb :: Duration -> Sound T Pulse -> Sound T Pulse
reverb d = convolveDuration kernel
  where
    kernel =
      Kernel
        { coefficients = \p -> coerce p ** 2,
          size = d,
          offset = d / 2
        }
{-# INLINE reverb #-}
