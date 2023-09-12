module LambdaSound.Cache (cache) where

import Codec.Compression.GZip (compress, decompress)
import Control.Monad.IO.Class (MonadIO (..))
import Data.ByteString (fromStrict, toStrict)
import Data.ByteString.Lazy qualified as BL
import Data.Hashable (hash)
import Data.Massiv.Array qualified as M
import Data.Massiv.Array.Unsafe qualified as MU
import Data.Vector.Storable qualified as V
import Data.Vector.Storable.ByteString (byteStringToVector, vectorToByteString)
import Data.Word
import LambdaSound.Sound
import LambdaSound.Sound.ComputeSound
import LambdaSound.Sound.Types
import System.Directory
import System.FilePath (joinPath)

-- | Caches a sound. If the sound is cached, then
-- the sound gets read from the XDG data directory and does not have to
-- be computed again.
-- It might load a cached sound which is not the same
-- as the computed one, but this should be very unlikely
cache :: Sound d Pulse -> Sound d Pulse
cache (TimedSound d msc) = TimedSound d $ cacheComputation msc
cache (InfiniteSound msc) = InfiniteSound $ cacheComputation msc

cacheComputation :: ComputeSound Pulse -> ComputeSound Pulse
cacheComputation cs = ComputeSound $ \si memo -> do
  key <- liftIO $ computeCacheKey cs
  cacheDir <- liftIO $ getXdgDirectory XdgCache "lambdasound"
  let directoryPath = joinPath [cacheDir, show key]
  liftIO $ createDirectoryIfMissing True directoryPath

  let filePath = joinPath [directoryPath, show $ si.samples]

  exists <- liftIO $ doesFileExist filePath
  if exists
    then do
      file <- liftIO $ BL.readFile filePath
      let floats = byteStringToVector $ toStrict $ decompress file
          (ComputeSound compute) = makeWithIndexFunction @Pulse (\_ index -> floats V.! index)
      compute si memo
    else 
    do
      (writeResult, ci) <- asWriteResult cs si memo
      pure
        ( WriteResult $ \dest -> do
            writeResult dest
            floats <- MU.unsafeFreeze M.Seq dest
            let bytes = compress $ fromStrict $ vectorToByteString $ M.toStorableVector floats
            BL.writeFile filePath bytes,
          ci
        )

computeCacheKey :: ComputeSound Pulse -> IO Word64
computeCacheKey cs = do
  let sr = makeSamplingInfo 50 1
  floats <- sampleComputeSound sr cs
  pure $ fromIntegral $ hash $ M.toList $ M.map (* 1000) floats
