-- | This module contains functions to sample sound and to save it in a file.
-- The @LambdaSound.Play@ module exports a function to play sounds directly.
module LambdaSound.Sampling (sampleSound, sampleSoundRaw, unsampleSound, unsampleSoundWithHz) where

import Data.Coerce (coerce)
import Data.Massiv.Array qualified as M
import Data.Massiv.Array.Unsafe qualified as MU
import LambdaSound.Sound
import LambdaSound.Sound.ComputeSound (sampleComputeSound)
import LambdaSound.Sound.Types (makeSamplingInfo)

-- | Samples a sound with the given frequency (usually 44100 is good) without post-processing
sampleSoundRaw :: Hz -> Sound T Pulse -> IO (M.Vector M.S Pulse)
sampleSoundRaw hz (TimedSound duration msc) = do
  let sr = makeSamplingInfo hz duration
  sampleComputeSound sr msc

-- | Samples a sound with the given frequency (usually 44100 is good) with post-processing
--
-- This is recommended over 'sampleSoundRaw' if you are unsure
sampleSound :: Hz -> Sound T Pulse -> IO (M.Vector M.S Pulse)
sampleSound hz sound =
  M.compute . postProcess <$> sampleSoundRaw hz sound

postProcess :: (M.Source r Pulse) => M.Vector r Pulse -> M.Vector M.D Pulse
postProcess = compressDynamically

-- | Apply dynamic compression on a vector of samples such that
-- they are constrained within (-1, 1). Quieter sounds are boosted
-- while louder sounds are reduced.
-- This is very important if you use the parallel combinator since
-- parallel sounds are awful without post processing.
compressDynamically :: (M.Source r Pulse) => M.Vector r Pulse -> M.Vector M.D Pulse
compressDynamically signal = M.map (scaleToMax . sigmoid) signal
  where
    scaleToMax x = (1 / sigmoid maxPulse) * x
    sigmoid x = 2 / (1 + exp (g * (-x))) - 1
    g = logBase (2 - factor) factor / (-maxPulse)
    maxPulse = M.maximum' $ M.map abs signal
    factor = 0.8

-- | Convert a vector of samples into the 'Sound' datatype so that it can be transformed
-- with the various combinators.
--
-- Keep in mind that if you do not exactly match the size of
-- the vector and the needed amount of samples for the sound,
-- then its speed will actually change which will also affect the sound pitch.
-- Also, sub- or supersampling will happen.
unsampleSound :: (M.Source r Pulse) => M.Vector r Pulse -> Sound I Pulse
unsampleSound samples = makeSound $ \si ->
  if M.unSz (M.size samples) == si.samples
    then MU.unsafeIndex samples
    else
      let scaler :: Double = fromIntegral (M.unSz $ M.size samples) / fromIntegral si.samples
       in \i -> MU.unsafeIndex samples $ floor (fromIntegral i * scaler)

-- | Convert a vector of samples into the 'Sound' datatype so that it can be transformed
-- with the various combinators.
--
-- Given the sample rate used for the creation of the vector, the resulting sound
-- will have the same duration as the original sound. However, sub- and supersampling will still happen
-- similarly to `unsampleSound` and changing the 'Duration' also changes the pitch.
unsampleSoundWithHz :: (M.Source r Pulse) => Hz -> M.Vector r Pulse -> Sound T Pulse
unsampleSoundWithHz hz samples = setDuration d $ unsampleSound samples
  where
    d = coerce $ fromIntegral (M.unSz $ M.size samples) / hz
