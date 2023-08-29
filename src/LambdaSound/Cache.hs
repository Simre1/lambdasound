module LambdaSound.Cache (cache) where

import Data.Binary (decode, encode)
import Data.ByteString.Lazy qualified as BL
import Data.Coerce (coerce)
import Data.IORef (newIORef, readIORef, writeIORef)
import Data.Vector qualified as V
import Data.Word
import GHC.IO (unsafePerformIO)
import LambdaSound.Sound
import System.Directory
import System.FilePath (joinPath)

-- | Caches a sound. If the sound is cached, then
-- the sound gets read from the XDG data directory and does not have to
-- be computed again. Might load a cached sound which is not the same
-- as the computed one, but this should be very unlikely
cache :: Sound d Pulse -> Sound d Pulse
cache = unsafePerformIO . cacheIO

cacheIO :: Sound d Pulse -> IO (Sound d Pulse)
cacheIO (TimedSound d c) = TimedSound d <$> cacheComputation c
cacheIO (InfiniteSound c) = InfiniteSound <$> cacheComputation c

cacheComputation :: (SampleRate -> CurrentSample -> Pulse) -> IO (SampleRate -> CurrentSample -> Pulse)
cacheComputation c = do
  let key :: Word64 = cacheKey c
  cacheDir <- getXdgDirectory XdgCache "lambdasound"
  let directoryPath = joinPath [cacheDir, show key]
  createDirectoryIfMissing True directoryPath

  fileContent <- newIORef Nothing
  pure $ \sr cs -> unsafePerformIO $ do
    let filePath = joinPath [directoryPath, show $ sr.samples]

    maybeFloats <- readIORef fileContent
    case maybeFloats of
      Just floats -> pure $ Pulse $ floats V.! cs.index
      Nothing -> do
        exists <- doesFileExist filePath
        if exists
          then do
            file <- BL.readFile filePath
            let floats = V.fromList $ decode file
            writeIORef fileContent $ Just floats
            pure $ Pulse $ floats V.! cs.index
          else
            let pulses = V.generate sr.samples $ c sr . coerce
             in do
                  BL.writeFile filePath $ encode @[Float] $ coerce $ V.toList pulses
                  pure $ pulses V.! cs.index

cacheKey :: (SampleRate -> CurrentSample -> Pulse) -> Word64
cacheKey c =
  let sr = SampleRate 1 50
   in sum $ round . (* 1000) . c sr . CurrentSample <$> [0 .. 49]