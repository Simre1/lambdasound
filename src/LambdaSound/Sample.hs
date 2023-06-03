module LambdaSound.Sample where

import Data.Coerce
import LambdaSound.Sound

newtype Hz = Hz Float deriving (Show, Eq, Ord, Num, Fractional, Floating, Enum)

pulse :: Hz -> Duration -> Sound Pulse
pulse hz d  = (\t -> sin (coerce hz * coerce t * 2 * pi)) <$> time d

harmonic :: Hz -> Duration -> Sound Pulse
harmonic hz d = parallel $ (\x -> reduce x $ pulse (coerce x * hz) d) <$> take 6 [1..]

harmonic2 :: Hz -> Duration -> Sound Pulse
harmonic2 hz d = parallel $ (\x -> reduce (logBase 2 (x + 1)) $ pulse (coerce x * hz) d) <$> take 6 [1..]

