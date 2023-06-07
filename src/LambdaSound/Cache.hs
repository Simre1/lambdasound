module LambdaSound.Cache (cache) where

import Data.Word
import LambdaSound.Sound
import System.Directory
import GHC.IO (unsafePerformIO)
import Data.ByteString.Lazy qualified as BL
import System.FilePath (joinPath)
import Data.Binary (decode, encode)
import qualified Data.Vector as V
import Data.Coerce (coerce)

-- | Caches a sound. If the sound is cached, then 
-- the sound gets read from the XDG data directory and does not have to
-- be computed again. Might load a cached sound which is not the same
-- as the computed one, but this should be very unlikely
cache :: Sound Pulse -> Sound Pulse
cache = unsafePerformIO . cacheIO

cacheIO :: Sound Pulse -> IO (Sound Pulse)
cacheIO (Sound d e c) = do
  cacheDir <- getXdgDirectory XdgCache "lambdasound"
  let key :: Word64 = fromIntegral $ sum $ e <$> cachingTestValues
  let filePath = joinPath [cacheDir, show key]
  createDirectoryIfMissing True cacheDir
  exists <- doesFileExist filePath
  if exists
    then do
      file <- BL.readFile filePath
      let floats = V.fromList $ decode file
      pure $ Sound d e $ \_ i -> Pulse $ floats V.! i
    else pure $ Sound d e $ \t ->
      let pulses = V.generate (V.length t) (c t)
      in unsafePerformIO $ do
        BL.writeFile filePath $ encode @[Float] $ coerce $ V.toList pulses
        pure $ \i -> pulses V.! i

cachingTestValues :: [Int]
cachingTestValues = [1,-1, 1000, -10000, 10000000000]