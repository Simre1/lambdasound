{-# LANGUAGE TemplateHaskellQuotes #-}
module LambdaSound.Cache (cache) where

import Data.Word
import LambdaSound.Sound
import System.Random
import System.Directory
import Language.Haskell.TH
import Control.Monad.IO.Class
import GHC.IO (unsafePerformIO)
import Data.ByteString.Lazy qualified as BL
import System.FilePath (joinPath)
import Data.Binary (decode, encode)
import qualified Data.Vector as V
import Data.Coerce (coerce)

-- | Caches a sound. If this TH quote does not get recompiled, then 
-- the sound gets read from the XDG data directory and does not have to
-- be computed again. Computation-intensive sounds should therefore 
-- be placed in a separate module.
cache :: Code Q (Sound Pulse -> Sound Pulse)
cache = Code $ do
  key <- liftIO randomIO
  examineCode [|| cacheIO key ||]

cacheIO :: Word64 -> Sound Pulse -> Sound Pulse
cacheIO key (Sound d c) = unsafePerformIO $ do
  cacheDir <- getXdgDirectory XdgCache "lambdasound"
  let filePath = joinPath [cacheDir, show key]
  createDirectoryIfMissing True cacheDir
  exists <- doesFileExist filePath
  if exists
    then do
      file <- BL.readFile filePath
      let floats = V.fromList $ decode file
      pure $ Sound d $ \_ i -> Pulse $ floats V.! i
    else pure $ Sound d $ \t ->
      let pulses = V.generate (V.length t) (c t)
      in unsafePerformIO $ do
        BL.writeFile filePath $ encode @[Float] $ coerce $ V.toList pulses
        pure $ \i -> pulses V.! i
