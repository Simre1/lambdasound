module LambdaSound.Sampling (sampleSound, sampleSoundRaw, saveWav, saveRawPCM) where

import Codec.Audio.Wave
import Data.ByteString.Builder qualified as B
import Data.Coerce
import Data.Massiv.Array qualified as M
import LambdaSound.Samples
import LambdaSound.Sound

-- | Samples a sound with the given frequency (usually 44100 is good) without post-processing
sampleSoundRaw :: Hz -> Sound T Pulse -> M.Vector M.D Pulse
sampleSoundRaw hz (TimedSound duration compute) =
  let period = coerce $ 1 / hz
      sr = SampleRate period (round $ coerce duration / period)
      compute' = compute sr
   in M.generate M.Seq (M.Sz1 sr.samples) $
        compute' . coerce

-- | Samples a sound with the given frequency (usually 44100 is good) with post-processing
--
-- This is recommended over 'sampleSoundRaw' if you are unsure
sampleSound :: Hz -> Sound T Pulse -> M.Vector M.S Pulse
sampleSound hz sound =
  M.compute $ postProcess $ sampleSoundRaw hz sound

postProcess :: (M.Source r Pulse) => M.Vector r Pulse -> M.Vector M.D Pulse
postProcess = compressDynamically

-- | Save the sound as raw floats
saveRawPCM :: FilePath -> M.Vector M.S Pulse -> IO ()
saveRawPCM filePath floats =
  B.writeFile filePath $ M.foldMono (B.floatLE . coerce) floats

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

-- | Save a sound to a wave file with the given sampling frequency
saveWav :: FilePath -> Int -> M.Vector M.S Pulse -> IO ()
saveWav filepath sampleRate floats = do
  let floatsLength = M.unSz $ M.size floats
      wave =
        Wave
          { waveFileFormat = WaveVanilla,
            waveSampleRate = fromIntegral sampleRate,
            waveSampleFormat = SampleFormatIeeeFloat32Bit,
            waveChannelMask = speakerMono,
            waveDataOffset = 0,
            waveDataSize = fromIntegral floatsLength * 4,
            waveSamplesTotal = fromIntegral floatsLength,
            waveOtherChunks = []
          }
  writeWaveFile filepath wave $ \handle ->
    B.hPutBuilder handle $ M.foldMono (B.floatLE . coerce) floats
