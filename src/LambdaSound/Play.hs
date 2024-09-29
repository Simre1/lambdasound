{-# LANGUAGE TypeAbstractions #-}
module LambdaSound.Play where

-- import qualified SDL.Raw.Audio as SDL

import Control.Concurrent.Chan.Unagi.NoBlocking
import Control.Monad (forM_, when, forever)
import Data.Vector.Storable qualified as VS
import Data.Vector.Storable.Mutable qualified as VSM
import Foreign
import LambdaSound.Compute
import LambdaSound.Music
import LambdaSound.Sound
import SDL qualified as SDL
import SDL.Raw qualified as SDL (queueAudio)
import Control.Concurrent (threadDelay)
import Data.Data (Typeable)
import Type.Reflection
import Foreign.C
import LambdaSound.SIMDRoutines (MyFloat)

play :: Hz -> Music -> IO ()
play hz music = do
  (inChan, outChan) <- newChan
  SDL.initialize [SDL.InitAudio]
  (device, spec) <-
    SDL.openAudioDevice $
      SDL.OpenDeviceSpec
        { openDeviceFreq = SDL.Mandate $ fromIntegral hz,
          openDeviceFormat = SDL.Mandate SDL.FloatingNativeAudio,
          openDeviceChannels = SDL.Mandate SDL.Mono,
          openDeviceSamples = fromIntegral bufferSize,
          openDeviceCallback = \format buffer -> do
            next <- tryReadChan outChan >>= tryRead
            case next of
              Just queuedSamples -> do
                written <- writeSamples queuedSamples format buffer
                when (written < VS.length queuedSamples) $
                  putStrLn "Warning: SDL Audio buffer too small for all samples"
              Nothing -> writeSilence format buffer,
          openDeviceUsage = SDL.ForPlayback,
          openDeviceName = Nothing
        }
  SDL.setAudioDevicePlaybackState device SDL.Play

  Samples samples <- computeMusic hz music
  
  let blocks = VS.length samples `quot` bufferSize
  forM_ [0..blocks-1] $ \i -> do
    let sampleBlock = VS.slice (i * bufferSize) bufferSize samples
    writeChan inChan $ sampleBlock

  let finalSampleBlock = VS.drop (blocks * bufferSize) samples
  writeChan inChan $ finalSampleBlock

  forever (threadDelay 10000)
  -- waitWhileActive (isActive outChan) -- not sure if this is right
  where
    bufferSize :: Int
    bufferSize = nextPowerOf2 $ fromIntegral $ hz `quot` 50 -- around 20 ms
  

waitWhileActive :: IO Bool -> IO ()
waitWhileActive check = do
  active <- check
  threadDelay 200
  if active then waitWhileActive check else pure () 

writeSamples :: VS.Vector MyFloat -> SDL.AudioFormat a -> VSM.IOVector a -> IO Int
writeSamples samples format buffer = do
  case format of
    SDL.Signed8BitAudio -> write round buffer
    SDL.Unsigned8BitAudio -> write round buffer
    SDL.Signed16BitLEAudio -> write round buffer
    SDL.Signed16BitBEAudio -> write round buffer
    SDL.Signed16BitNativeAudio -> write round buffer
    SDL.Unsigned16BitLEAudio -> write round buffer
    SDL.Unsigned16BitBEAudio -> write round buffer
    SDL.Unsigned16BitNativeAudio -> write round buffer
    SDL.Signed32BitLEAudio -> write round buffer
    SDL.Signed32BitBEAudio -> write round buffer
    SDL.Signed32BitNativeAudio -> write round buffer
    SDL.FloatingLEAudio -> write realToFrac buffer
    SDL.FloatingBEAudio -> write realToFrac buffer
    SDL.FloatingNativeAudio -> write realToFrac buffer
    -- TODO: Currently it is assumed that endianness is correct. Should need to verify this before writing the samples.
  where
    write :: (Storable a) => (MyFloat -> a) -> VSM.IOVector a -> IO Int
    write cast buffer = do
      let writeSize = min (VS.length samples) (VSM.length buffer)
      forM_ [0 .. writeSize - 1] $ \i -> VSM.write buffer i (cast $ samples VS.! i)
      pure writeSize

writeSilence :: SDL.AudioFormat a -> VSM.IOVector a -> IO ()
writeSilence format buffer = do
  case format of
    SDL.Signed8BitAudio -> write buffer
    SDL.Unsigned8BitAudio -> write buffer
    SDL.Signed16BitLEAudio -> write buffer
    SDL.Signed16BitBEAudio -> write buffer
    SDL.Signed16BitNativeAudio -> write buffer
    SDL.Unsigned16BitLEAudio -> write buffer
    SDL.Unsigned16BitBEAudio -> write buffer
    SDL.Unsigned16BitNativeAudio -> write buffer
    SDL.Signed32BitLEAudio -> write buffer
    SDL.Signed32BitBEAudio -> write buffer
    SDL.Signed32BitNativeAudio -> write buffer
    SDL.FloatingLEAudio -> write buffer
    SDL.FloatingBEAudio -> write buffer
    SDL.FloatingNativeAudio -> write buffer
  where
    write :: (Storable a, Num a) => VSM.IOVector a -> IO ()
    write buffer = do
      forM_ [0 .. VSM.length buffer - 1] $ \i -> VSM.write buffer i 0

nextPowerOf2 :: Integral a => a -> a
nextPowerOf2 n
  | n < 1 = 1 -- Handles the case where n is less than 1
  | otherwise = 2 ^ ceiling (logBase 2 (fromIntegral n :: Double))
