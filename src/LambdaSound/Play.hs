module LambdaSound.Play (sampleSound, play, saveWav, saveRawPCM) where

import Codec.Audio.Wave
import Control.Concurrent (threadDelay)
import Control.Monad (guard, when)
import Data.ByteString (toStrict)
import Data.ByteString.Builder qualified as B
import Data.Coerce
import Data.Vector qualified as V
import LambdaSound.Sample
import LambdaSound.Sound
import Sound.ProteaAudio.SDL qualified as PA

-- | Samples a sound with the given frequency (usually 44100 is good)
sampleSound :: Hz -> Sound T a -> V.Vector a
sampleSound hz (TimedSound duration compute) =
  let period = coerce $ 1 / hz
      sr = SampleRate period (round $ coerce duration / period)
   in V.generate sr.samples $ compute sr . coerce

-- | Save the sound as raw floats
saveRawPCM :: Hz -> Sound T Pulse -> FilePath -> IO ()
saveRawPCM hz sound filePath =
  let floats = compressDynamically $ coerce $ sampleSound hz sound
   in B.writeFile filePath $ foldMap B.floatLE floats

-- | Apply dynamic compression on a vector of samples such that
-- they are constrained within (-1, 1). Quieter sounds are boosted
-- while louder sounds are reduced.
-- This is very important if you use the parallel combinator since
-- parallel sounds are awful without post processing.
compressDynamically :: V.Vector Float -> V.Vector Float
compressDynamically signal = scaleToMax . sigmoid <$> signal
  where
    scaleToMax x = (1 / sigmoid maxPulse) * x
    sigmoid x = 2 / (1 + exp (g * (-x))) - 1
    g = logBase (2 - factor) factor / (-maximum signal)
    maxPulse = maximum $ fmap abs signal
    factor = 0.8

-- | Play the sound with the given sample rate and the given volume.
-- 
-- You need to have SDL2 installed for playing! 
play :: Int -> Float -> Sound T Pulse -> IO ()
play sampleRate volume sound = do
  PA.initAudio 1 sampleRate 1024 >>= guard

  let floatBytes =
        B.toLazyByteString $
          foldMap B.floatLE $
            compressDynamically $
              coerce $
                sampleSound (realToFrac sampleRate) sound

  sample <- PA.sampleFromMemoryPcm (toStrict floatBytes) 1 sampleRate 32 volume
  _sound <- PA.soundPlay sample 1 1 0 1

  waitPlayback

  PA.finishAudio

waitPlayback :: IO ()
waitPlayback = do
  n <- PA.soundActiveAll
  when (n > 0) $ do
    threadDelay 1000
    waitPlayback

-- | Save a sound to a wave file with the given sampling frequency
saveWav :: FilePath -> Int -> Sound T Pulse -> IO ()
saveWav filepath hz sound = do
  let floats = compressDynamically $ coerce $ sampleSound (fromIntegral hz) sound
      wave =
        Wave
          { waveFileFormat = WaveVanilla,
            waveSampleRate = fromIntegral hz,
            waveSampleFormat = SampleFormatIeeeFloat32Bit,
            waveChannelMask = speakerMono,
            waveDataOffset = 0,
            waveDataSize = fromIntegral (V.length floats) * 4,
            waveSamplesTotal = fromIntegral $ V.length floats,
            waveOtherChunks = []
          }
  writeWaveFile filepath wave $ \handle ->
    B.hPutBuilder handle $ foldMap B.floatLE floats
