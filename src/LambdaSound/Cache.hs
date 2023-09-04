module LambdaSound.Cache (cache) where

import Codec.Compression.GZip (compress, decompress)
import Control.Monad.IO.Class (MonadIO (..))
import Data.ByteString.Lazy qualified as BL
import Data.Massiv.Array qualified as M
import Data.Vector.Storable qualified as V
import Data.Vector.Storable.ByteString (byteStringToVector, vectorToByteString)
import Data.Word
import LambdaSound.Sound
import LambdaSound.Sound.ComputeSound
import LambdaSound.Sound.MSC
import System.Directory
import System.FilePath (joinPath)
import Data.ByteString (fromStrict, toStrict)
import Data.Hashable (hash)

-- | Caches a sound. If the sound is cached, then
-- the sound gets read from the XDG data directory and does not have to
-- be computed again.
-- It might load a cached sound which is not the same
-- as the computed one, but this should be very unlikely
cache :: Sound d Pulse -> Sound d Pulse
cache (TimedSound d msc) = TimedSound d $ cacheComputation msc
cache (InfiniteSound msc) = InfiniteSound $ cacheComputation msc

cacheComputation :: MSC (ComputeSound Pulse) -> MSC (ComputeSound Pulse)
cacheComputation msc = do
  key <- liftIO $ computeCacheKey msc
  cacheDir <- liftIO $ getXdgDirectory XdgCache "lambdasound"
  let directoryPath = joinPath [cacheDir, show key]
  liftIO $ createDirectoryIfMissing True directoryPath

  sr <- getSR
  let filePath = joinPath [directoryPath, show $ sr.samples]

  exists <- liftIO $ doesFileExist filePath
  if exists -- exists
    then do
      file <- liftIO $ BL.readFile filePath
      let floats = byteStringToVector $ toStrict $ decompress file
      makeIndexCompute @Pulse (\_ index -> floats V.! index)
    else do
      (wm, ci) <- asWriteMemory msc
      pure $
        ComputeSound
          ( \_ chooseWm -> chooseWm $ \basePtr ptr -> do
              wm basePtr ptr
              floats <- vectorFromPtr ptr sr.samples
              let bytes = compress $ fromStrict $ vectorToByteString $ M.toStorableVector floats
              BL.writeFile filePath bytes
          )
          ci

computeCacheKey :: MSC (ComputeSound Pulse) -> IO Word64
computeCacheKey msc = do
  let sr = SampleRate 1 50
  floats <- sampleMSC sr msc
  pure $ fromIntegral $ hash $ M.toList $ M.map (* 1000) floats
