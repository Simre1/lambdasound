module LambdaSound.Sample where

import Data.Coerce
import Data.Vector qualified as V
import LambdaSound.Sound
import System.Random as R

newtype Hz = Hz Float deriving (Show, Eq, Ord, Num, Fractional, Floating, Enum)

pulse :: Hz -> Sound I Pulse
pulse hz = (\t -> sin (coerce hz * coerce t * 2 * pi)) <$> time

harmonic :: Hz -> Sound I Pulse
harmonic hz = parallel $ (\x -> reduce x $ pulse (coerce x * hz)) <$> take 6 [1 ..]

harmonic2 :: Hz -> Sound I Pulse
harmonic2 hz = parallel $ (\x -> reduce (logBase 2 (x + 1)) $ pulse (coerce x * hz)) <$> take 6 [1 ..]

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
