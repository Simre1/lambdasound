module LambdaSound.Create
  ( -- *** Basic sounds
    time,
    progress,
    sampleIndex,
    constant,
    silence,

    -- *** Iterating
    iterateSound,
    iterateSoundPulse,

    -- *** Unfolding
    unfoldlSound,
    unfoldlSoundPulse,
    unfoldrSound,
    unfoldrSoundPulse,
    iUnfoldlSound,
    iUnfoldlSoundPulse,
    iUnfoldrSound,
    iUnfoldrSoundPulse,
  )
where

import Data.Coerce (coerce)
import Data.Massiv.Array qualified as M
import LambdaSound.Sound

-- | A 'Sound' with @0@ volume
silence :: Sound I Pulse
silence = constant 0

-- | A constant 'Sound'
constant :: a -> Sound I a
constant a = makeSound $ const (const a)

-- | Iterate over the samples to create the sound.
-- 
-- The 'Pulse' version is faster then the non-'Pulse' version
iterateSoundPulse :: (Pulse -> Pulse) -> Pulse -> Sound I Pulse
iterateSoundPulse f s = fillWholeSound $ \si ->
  M.iterateN (M.Sz1 si.samples) f s

-- | Iterate over the samples to create the sound.
iterateSound :: (a -> a) -> a -> Sound I a
iterateSound f s = makeSoundVector $ \si ->
  M.delay $ M.compute @M.B $ M.iterateN (M.Sz1 si.samples) f s

-- | Unfold the samples of a sound from the start to the end
--
--  The 'Pulse' version is faster then the non-'Pulse' version
unfoldlSoundPulse :: (s -> (s, Pulse)) -> s -> Sound I Pulse
unfoldlSoundPulse f s = fillWholeSound $ \si ->
  M.unfoldlS_ (M.Sz1 si.samples) f s

-- | Unfold the samples of a sound from the start to the end
unfoldlSound :: (s -> (s, a)) -> s -> Sound I a
unfoldlSound f s = makeSoundVector $ \si ->
  M.delay $ M.compute @M.B $ M.unfoldlS_ (M.Sz1 si.samples) f s

-- | Unfold the samples of a sound from the end to start
--
-- The 'Pulse' version is faster then the non-'Pulse' version
unfoldrSoundPulse :: (s -> (Pulse, s)) -> s -> Sound I Pulse
unfoldrSoundPulse f s = fillWholeSound $ \si ->
  M.unfoldrS_ (M.Sz1 si.samples) f s

-- | Unfold the samples of a sound from the end to start
unfoldrSound :: (s -> (a, s)) -> s -> Sound I a
unfoldrSound f s = makeSoundVector $ \si ->
  M.delay $ M.compute @M.B $ M.unfoldrS_ (M.Sz1 si.samples) f s

-- | Unfold the samples of a sound from the start to the end with the index starting at 0
--
--  The 'Pulse' version is faster then the non-'Pulse' version
iUnfoldlSoundPulse :: (Int -> s -> (s, Pulse)) -> s -> Sound I Pulse
iUnfoldlSoundPulse f s = fillWholeSound $ \si ->
  M.iunfoldlS_ (M.Sz1 si.samples) f s

-- | Unfold the samples of a sound from the start to the end with the index starting at 0
iUnfoldlSound :: (Int -> s -> (s, a)) -> s -> Sound I a
iUnfoldlSound f s = makeSoundVector $ \si ->
  M.delay $ M.compute @M.B $ M.iunfoldlS_ (M.Sz1 si.samples) f s

-- | Unfold the samples of a sound from the end to the start with the index starting at 0
--
--  The 'Pulse' version is faster then the non-'Pulse' version
iUnfoldrSoundPulse :: (s -> Int -> (Pulse, s)) -> s -> Sound I Pulse
iUnfoldrSoundPulse f s = fillWholeSound $ \si ->
  M.iunfoldrS_ (M.Sz1 si.samples) f s

-- | Unfold the samples of a sound from the end to the start with the index starting at 0
iUnfoldrSound :: (s -> Int -> (a, s)) -> s -> Sound I a
iUnfoldrSound f s = makeSoundVector $ \si ->
  M.delay $ M.compute @M.B $ M.iunfoldrS_ (M.Sz1 si.samples) f s

-- | Get the time for each sample which can be used for sinus wave calculations (e.g. 'sineWave')
time :: Sound I Time
time = makeSound $ \si index ->
  coerce $ fromIntegral index * si.period

-- | Get the 'Progress' of a 'Sound'.
-- 'Progress' of '0' means that the sound has just started.
-- 'Progress' of '1' means that the sound has finished.
-- 'Progress' greater than '1' or smaller than '0' is invalid.
progress :: Sound I Progress
progress = makeSound $ \si index ->
  fromIntegral index / fromIntegral si.samples

-- | Tells you the sample index for each sample
sampleIndex :: Sound I Int
sampleIndex = makeSound (const id)
