module LambdaSound.Effect where

import Data.Coerce
import LambdaSound.Sound

easeInOut :: Int -> Sound d Pulse -> Sound d Pulse
easeInOut strength = zipSound $ (*) . f <$> progress
  where
    f p = coerce $ -(2 * p - 1) ** (abs (fromIntegral strength) * 2) + 1

repeatSound :: Int -> Sound T Pulse -> Sound T Pulse
repeatSound n s
  | n <= 0 = mempty
  | even n = s' >>> s'
  | otherwise = s' >>> s' >>> s
  where
    s' = repeatSound (n `quot` 2) s

reverb :: Duration -> Sound T Pulse -> Sound T Pulse
reverb d = convolveDuration kernel
  where
    kernel =
      Kernel
        { coefficients = \p -> coerce p ** 3,
          size = d,
          offset = -d / 2
        }
