module LambdaSound.Play (play) where

import Control.Concurrent (threadDelay)
import Control.Monad (guard, when)
import Data.Massiv.Array qualified as M
import Data.Vector.Storable.ByteString (vectorToByteString)
import LambdaSound.Sampling
import LambdaSound.Sound
import Sound.ProteaAudio.SDL qualified as PA

-- | Play the sound with the given sample rate and the given volume.
--
-- You need to have SDL2 installed for playing!
play :: Int -> Float -> Sound T Pulse -> IO ()
play sampleRate volume sound = do
  samples <- sampleSound (realToFrac sampleRate) sound
  playSamples sampleRate volume samples

playSamples :: Int -> Float -> M.Vector M.S Pulse -> IO ()
playSamples sampleRate volume samples = do
  PA.initAudio 1 sampleRate 1024 >>= guard

  let floatBytes =
        vectorToByteString $
          M.toStorableVector
            samples

  sample <- PA.sampleFromMemoryPcm floatBytes 1 sampleRate 32 volume
  _sound <- PA.soundPlay sample 1 1 0 1

  waitPlayback

  PA.finishAudio

waitPlayback :: IO ()
waitPlayback = do
  n <- PA.soundActiveAll
  when (n > 0) $ do
    threadDelay 1000
    waitPlayback
