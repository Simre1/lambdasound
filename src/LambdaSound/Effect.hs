module LambdaSound.Effect where

import Data.Coerce
import Data.Semigroup (stimes)
import Data.Vector qualified as V
import LambdaSound.Sound

easeInOut :: Int -> Sound Pulse -> Sound Pulse
easeInOut strength sound =
  zipSound ((*) <$> (f <$> progress (getDuration sound))) sound
  where
    f p = coerce $ -(2 * p - 1) ** (abs (fromIntegral strength) * 2) + 1

repeatSound :: Int -> Sound a -> Sound a
repeatSound = stimes

reverb :: Duration -> Sound Pulse -> Sound Pulse
reverb d = convolve kernel
  where
    kernel =
      Kernel
        { coefficients = \(start, end) ->
            let n = end - start
             in V.generate n $ \i -> fromIntegral (n - i) / fromIntegral n,
          size = d,
          offset = d / 2
        }
