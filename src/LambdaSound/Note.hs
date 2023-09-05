module LambdaSound.Note where

import LambdaSound.Sound

-- | Semitones are tones like 'c4', 'd4' or 'c5'.
-- The semitone is used to determine the hz of the tone based on 'pitchStandard'
newtype Semitone = Semitone Int deriving (Show, Eq, Num, Ord, Enum)

-- | 440 Hz is used at the pitch standard for the tone 'a4'
pitchStandard :: Hz
pitchStandard = 440.0

-- | Converts a semitone to the appropriate frequency based on 'pitchStandard'
semitoneToHz :: Semitone -> Hz
semitoneToHz n = pitchStandard * (2 ** (fromIntegral (fromEnum n) * 1.0 / 12.0))
{-# INLINE semitoneToHz #-}
-- | Raise a sound by the given amount of semitones.
-- This only works for sounds which use the period length given
-- in the compute step of the sound. 'pulse' works but 'noise' does not.
-- For example:
-- > raiseSemitones 2 (asNote pulse c3) = asNote pulse d3
raiseSemitones :: Int -> Sound d Pulse -> Sound d Pulse
raiseSemitones x = raise (2 ** (fromIntegral x / 12))
{-# INLINE raiseSemitones #-}

-- | Diminishes a sound by the given amount of semitones
diminishSemitones :: Int -> Sound d Pulse -> Sound d Pulse
diminishSemitones x = raiseSemitones (-x)
{-# INLINE diminishSemitones #-}

-- | Transforms a function taking a 'Hz' to one taking a 'Semitone'.
-- Should be used with 'pulse' or 'harmonic'
asNote :: (Hz -> a) -> Semitone -> a
asNote f s = f (semitoneToHz s)
{-# INLINE asNote #-}

c1, d1, e1, f1, g1, a1, b1 :: Semitone
c1 = -45
d1 = -43
e1 = -41
f1 = -40
g1 = -38
a1 = -36
b1 = -34

c2, d2, e2, f2, g2, a2, b2 :: Semitone
c2 = -33
d2 = -31
e2 = -29
f2 = -28
g2 = -26
a2 = -24
b2 = -22

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

c6, d6, e6, f6, g6, a6, b6 :: Semitone
c6 = 15
d6 = 17
e6 = 19
f6 = 20
g6 = 22
a6 = 24
b6 = 26

c7, d7, e7, f7, g7, a7, b7 :: Semitone
c7 = 27
d7 = 29
e7 = 31
f7 = 32
g7 = 34
a7 = 36
b7 = 38

-- ** Notes
-- | These are durations for the corresponding note lenghts
-- assuming 60 bpm.
-- 
-- If you know that a sound has 60 bpm, you can easily scale to 
-- different bpm with 'scaleDuration':
-- @
-- scaleDuration (wantedBPM / 60) soundWith60BPM
-- @
wholeNote, halfNote, quarterNote, eightNote :: Duration
wholeNote = 1
halfNote = 1 / 2
quarterNote = 1 / 4
eightNote = 1 / 8