module LambdaSound.Compute where

import Control.Monad (forM_)
import Data.Coerce (coerce)
import Data.Vector.Storable qualified as VS
import Data.Vector.Storable.Mutable qualified as VSM
import Foreign.C
import LambdaSound.Music
import LambdaSound.SIMDRoutines 
import LambdaSound.Sound

newtype Hz = Hz Int deriving (Num, Eq, Show, Ord, Integral, Enum, Real)

newtype Samples = Samples (VS.Vector MyFloat)

data Segment
  = Generate {generate :: Int -> MyFloat}
  | WriteBuffer (VSM.IOVector MyFloat -> IO ())

asWriteBuffer :: Segment -> VSM.IOVector MyFloat -> IO ()
asWriteBuffer (WriteBuffer f) v = f v
asWriteBuffer (Generate f) v = forM_ [1 .. VSM.length v] $ \i -> do
  VSM.write v i (f i)

asGenerate :: Int -> Segment -> IO (Int -> MyFloat)
asGenerate _ (Generate f) = pure f
asGenerate samples (WriteBuffer f) = do
  vec <- VSM.new samples
  f vec
  frozen <- VS.unsafeFreeze vec
  pure $ \i -> frozen VS.! i

computeMusic :: Hz -> Music -> IO Samples
computeMusic (Hz hz) (Music {duration, sound}) = do
  setupSIMDRoutines
  let samples = round $ coerce @_ @Float duration * fromIntegral hz
      samplingFrequency = 1 / fromIntegral hz
  segment <- go samples samplingFrequency sound
  case segment of
    WriteBuffer f ->
      Samples <$> do
        vec <- VSM.unsafeNew samples
        f vec
        VS.unsafeFreeze vec
    Generate generate -> pure $ Samples $ VS.generate samples generate
  where
    go :: Int -> Float -> Sound -> IO Segment
    go samples samplingFrequency sound = case sound of
      ElementarySound (ConstantValue v) -> pure $
        Generate $
          \_ -> realToFrac v
      ElementarySound (SineWave {amplitude, hz}) -> pure $
        WriteBuffer $
          \buffer -> VSM.unsafeWith buffer $ \ptr -> simpleSin ptr (fromIntegral $ VSM.length buffer) (realToFrac amplitude) (realToFrac $ hz * samplingFrequency)
        -- Generate $
        --     \i -> realToFrac $ amplitude * sin (hz * 2 * pi * fromIntegral i * samplingFrequency)
      Sequence {seperator, sound1, sound2} -> do
        let samples1 = round @Float $ coerce seperator * fromIntegral samples
            samples2 = samples - samples1
        write1 <- asWriteBuffer <$> go samples1 samplingFrequency sound1
        write2 <- asWriteBuffer <$> go samples2 samplingFrequency sound2
        pure $ WriteBuffer $ \vec -> write1 vec *> write2 (VSM.drop samples1 vec)
      Elementwise {operation} ->
        let gen s = go samples samplingFrequency s >>= asGenerate samples
            binOp op sound1 sound2 = do
              s1 <- gen sound1
              s2 <- gen sound2
              pure $ op <$> s1 <*> s2
         in Generate <$> case operation of
              Add sound1 sound2 -> binOp (+) sound1 sound2
              Subtract sound1 sound2 -> binOp (-) sound1 sound2
              Multiply sound1 sound2 -> binOp (*) sound1 sound2
              Divide sound1 sound2 -> binOp (/) sound1 sound2
              Abs sound1 -> fmap abs <$> gen sound1
              Signum sound1 -> fmap signum <$> gen sound1
      Convolution {kernel} -> error "not supported yet"

-- sampleMusic :: Hz -> Music -> IO (VS.Vector Float)
-- sampleMusic (Hz hz) (Music {duration, sound}) = go sound
-- case sound of
--   ElementarySound
