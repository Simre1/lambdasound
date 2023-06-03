module LambdaSound.Play where
import LambdaSound.Sound
import LambdaSound.Sample
import Data.Coerce
import Data.ByteString.Builder qualified as B
import System.Process.Typed
import qualified Data.Vector as V

sampleSound :: Hz -> Sound a -> V.Vector a
sampleSound hz (Sound duration compute) =
  let step = 1 / hz
      i = V.fromList $ coerce [0, step..coerce duration]
      f = compute i
   in V.generate (V.length i) f

save :: Hz -> Sound Pulse -> FilePath -> IO ()
save hz sound filePath =
  let floats = compressDynamically $ realToFrac  <$> sampleSound hz sound
   in B.writeFile filePath $ foldMap B.floatLE floats

compressDynamically :: V.Vector Float -> V.Vector Float
compressDynamically signal = scaleToMax . sigmoid <$> signal
  where
    scaleToMax x = (1 / sigmoid maxPulse) * x
    sigmoid x = 2 / (1 + exp (g * (-x))) - 1
    g = logBase (2 - factor) factor / (-maximum signal)
    maxPulse = maximum $ fmap abs signal
    factor = 0.8

play :: Hz -> Sound Pulse -> IO ()
play sampleRate sound = do
  save sampleRate sound "/tmp/haskell-music"
  _ <-
    runProcess_ $
      setStderr nullStream $
        setStdout nullStream $
          shell $
            "ffplay -loglevel quiet -autoexit -showmode 1 -f f32le -ar "
              ++ show (round @Float @Int $ coerce sampleRate)
              ++ " /tmp/haskell-music"
  return ()