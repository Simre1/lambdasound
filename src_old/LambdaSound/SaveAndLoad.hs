-- | This module exports some functions for simple loading and saving sounds. 
--
-- However, keep in mind that loaded sounds have a fixed amound of samples and thus cannot be stretched
-- in duration losslessly. Stretching a sound results in sub- or supersampling and will also shift the pitch.
-- Pitch-retaining stretching has not been implemented yet!
module LambdaSound.SaveAndLoad
  ( saveWav,
    loadWav,
    saveRaw,
    loadRaw,
    saveRawCompressed,
    loadRawCompressed,
  )
where

import Data.List.NonEmpty
import LambdaSound.Sampling (sampleSound, unsampleSound, unsampleSoundWithHz)
import LambdaSound.SaveAndLoad.RawSamples qualified as RS
import LambdaSound.Sound

-- | Save a sound as a wav file using default 'sampleSound'.
saveWav :: FilePath -> Hz -> Sound T Pulse -> IO ()
saveWav filePath hz sound = do
  floats <- sampleSound hz sound
  RS.saveWav filePath hz floats

-- | Load a wav as a sound, mixing channels with 'parallel2'.
--
-- If you want to to use this with `embedIO`, you should probably use `embedIOLazily` instead!
loadWav :: FilePath -> IO (Sound T Pulse)
loadWav filePath = do
  (hz, channels) <- RS.loadWav filePath
  pure $ unsampleSoundWithHz hz $ Data.List.NonEmpty.head channels

-- | Save a sound as floats.
saveRaw :: FilePath -> Hz -> Sound T Pulse -> IO ()
saveRaw filePath hz sound = do
  floats <- sampleSound hz sound
  RS.saveRaw filePath floats

-- | Load a sound from floats.
--
-- If you want to to use this with `embedIO`, you should probably use `embedIOLazily` instead!
loadRaw :: FilePath -> IO (Sound I Pulse)
loadRaw filePath = do
  floats <- RS.loadRaw filePath
  pure $ unsampleSound floats

-- | Save a sound as gzip-compressed floats.
saveRawCompressed :: FilePath -> Hz -> Sound T Pulse -> IO ()
saveRawCompressed filePath hz sound = do
  floats <- sampleSound hz sound
  RS.saveRawCompressed filePath floats

-- | Load a sound from gzip-compressed floats.
--
-- If you want to to use this with `embedIO`, you should probably use `embedIOLazily` instead!
loadRawCompressed :: FilePath -> IO (Sound I Pulse)
loadRawCompressed filePath = do
  floats <- RS.loadRawCompressed filePath
  pure $ unsampleSound floats
