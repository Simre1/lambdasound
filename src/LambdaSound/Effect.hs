module LambdaSound.Effect where

import Data.Coerce
import LambdaSound.Sound

-- | Eases the volume of the sound. The given 'Int' controls the strength of the easing.
easeInOut :: Int -> Sound d Pulse -> Sound d Pulse
easeInOut strength = zipSoundWith (\p -> (f p *)) progress
  where
    f p = coerce $ -(2 * p - 1) ** (abs (fromIntegral strength) * 2) + 1

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

-- | Plays the sound multiple times to get a simple reverb effect. The duration specifies the length of the reverb.
simpleReverb :: Duration -> Sound T Pulse -> Sound T Pulse
simpleReverb duration sound = flip foldMap (zip [1..] [0,(duration / 4)..duration]) $ \(v,d) ->
  reduce v (setDuration d silence >>> sound) 

-- | ADSR envelope which specifies how the volume of a sound should behave over time
data Envelope = Envelope
  { attack :: !Duration,
    decay :: !Duration,
    release :: !Duration,
    sustain :: !Float
  }
  deriving (Eq, Show)

-- | Apply an ADSR envelope to a sound
applyEnvelope :: Envelope -> Sound T Pulse -> Sound T Pulse
applyEnvelope envelope sound =
  let attack = coerce <$> progress
      decay = fmap (\p -> coerce envelope.sustain + (1 - coerce p) ** 3 * (1 - coerce envelope.sustain)) progress
      sustain = constant (coerce envelope.sustain)
      release = fmap (\p -> coerce envelope.sustain * (1 - coerce p) ** 3) progress
      adsrCurve =
        sequentially
          [ envelope.attack |-> attack,
            envelope.decay |-> decay,
            (getDuration sound - envelope.release - envelope.decay - envelope.attack) |-> sustain,
            envelope.release |-> release
          ]
   in zipSoundWith (*) adsrCurve sound

-- | Add some harmonic frequencies
harmonic :: (Hz -> Sound I Pulse) -> Hz -> Sound I Pulse
harmonic f hz = parallel $ (\x -> reduce x $ f (coerce x * hz)) <$> take 6 [1 ..]
