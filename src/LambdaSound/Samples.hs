module LambdaSound.Samples where

import Data.Coerce
import Data.Fixed (mod')
import Data.Vector.Storable qualified as V
import LambdaSound.Sound
import System.Random as R
import Debug.Trace (traceShowId)


-- | Pure sinus sound
-- Warm and round
sineWave :: Hz -> Sound I Pulse
sineWave hz = (\t -> sin (coerce hz * coerce t * 2 * pi)) <$> time


-- | Triangle wave
-- Similar to sine but colder
triangleWave :: Hz -> Sound I Pulse
triangleWave hz =
  fmap
    ( \t ->
        let x = (coerce hz * coerce t) `mod'` 1
         in if x < 0.5
              then x * 4 - 1
              else 3 - x * 4
    )
    time

-- | Produces a sawtooth wave
-- Warm and sharp
sawWave :: Hz -> Sound I Pulse
sawWave hz = (\t -> (coerce hz * coerce t * 2) `mod'` 2 - 1) <$> time

-- | Produces a square wave
-- Cold
squareWave :: Hz -> Sound I Pulse
squareWave hz = (\t -> fromIntegral @Int $ round ((coerce hz * t) `mod'` 1) * 2 - 1) <$> time

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

