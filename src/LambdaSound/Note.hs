module LambdaSound.Note where

import LambdaSound.Sound
import LambdaSound.Sample

newtype Semitone = Semitone Int deriving (Show, Eq, Num, Ord, Enum)

pitchStandard :: Hz
pitchStandard = 440.0

semitoneToHz :: Semitone -> Hz
semitoneToHz n = pitchStandard * (2 ** (fromIntegral (fromEnum n) * 1.0 / 12.0))

raiseSemitones :: Int -> Sound d Pulse -> Sound d Pulse
raiseSemitones x = raise (2 ** (fromIntegral x / 12))

diminishSemitones :: Int -> Sound d Pulse -> Sound d Pulse
diminishSemitones x = raiseSemitones (-x)

asNote :: (Hz -> a) -> Semitone -> a
asNote f s = f (semitoneToHz s)

c3, d3, e3, f3, g3, a3, b3 :: Semitone
c3 = -21
d3 = -19
e3 = -17
f3 = -16
g3 = -14
a3 = -12
b3 = -10

c4, d4, e4, f4, g4, a4, b4 :: Semitone
c4 = -9
d4 = -7
e4 = -5
f4 = -4
g4 = -2
a4 = 0
b4 = 2

c5, d5, e5, f5, g5, a5, b5 :: Semitone
c5 = 3
d5 = 5
e5 = 7
f5 = 8
g5 = 10
a5 = 12
b5 = 14