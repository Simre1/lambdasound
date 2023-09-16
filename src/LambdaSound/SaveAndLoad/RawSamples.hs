{-# LANGUAGE AllowAmbiguousTypes #-}

module LambdaSound.SaveAndLoad.RawSamples
  ( saveWav,
    saveRaw,
    saveRawCompressed,
    loadWav,
    loadRaw,
    loadRawCompressed,
  )
where

import Codec.Audio.Wave
import Codec.Compression.GZip (compress, decompress)
import Control.Monad.IO.Class (liftIO)
import Data.ByteString qualified as B
import Data.ByteString.Lazy qualified as BL
import Data.Functor ((<&>))
import Data.Int (Int16, Int32, Int64)
import Data.List.NonEmpty
import Data.Massiv.Array qualified as M
import Data.Semigroup (Max (..))
import Data.Vector.Storable.ByteString (byteStringToVector, vectorToByteString)
import Data.Word (Word8)
import LambdaSound.Sound (Hz, Pulse)

-- | Save sound samples to a wave file with the given sampling frequency
saveWav :: FilePath -> Hz -> M.Vector M.S Pulse -> IO ()
saveWav filepath sampleRate floats = do
  let floatsLength = M.unSz $ M.size floats
      wave =
        Wave
          { waveFileFormat = WaveVanilla,
            waveSampleRate = round sampleRate,
            waveSampleFormat = SampleFormatIeeeFloat32Bit,
            waveChannelMask = speakerMono,
            waveDataOffset = 0,
            waveDataSize = fromIntegral floatsLength * 4,
            waveSamplesTotal = fromIntegral floatsLength,
            waveOtherChunks = []
          }
  writeWaveFile filepath wave $ \handle ->
    B.hPut handle $ vectorToByteString $ M.toStorableVector floats

-- | Load a wave file to get the sampling frequencies and the sound samples for the channels.
loadWav :: FilePath -> IO (Hz, NonEmpty (M.Vector M.S Pulse))
loadWav filePath = do
  wave <- readWaveFile filePath
  file <- B.readFile filePath
  let sourceVector = readSource wave $ B.drop (fromIntegral $ waveDataOffset wave) file
  pure (fromIntegral $ waveSampleRate wave, splitInChannels wave sourceVector)
  where
    splitInChannels :: Wave -> M.Vector M.D Pulse -> NonEmpty (M.Vector M.S Pulse)
    splitInChannels wave sourceVector =
      let channels = fromIntegral $ waveChannels wave
       in fromList $
            if channels == 1
              then [M.compute sourceVector]
              else
                [0 .. pred channels] <&> \channelOffset ->
                  M.compute $ M.downsample (M.Stride channels) $ M.drop (M.Sz1 channelOffset) sourceVector
    readSource :: Wave -> B.ByteString -> M.Vector M.D Pulse
    readSource wave sampleData =
      case waveSampleFormat wave of
        SampleFormatIeeeFloat32Bit -> mapAndLoad @Float
        SampleFormatIeeeFloat64Bit -> mapAndLoad @Double
        SampleFormatPcmInt 8 -> M.map ((+ (-1)) . (* 2)) $ mapAndLoad @Word8
        SampleFormatPcmInt 16 -> mapAndLoad @Int16
        SampleFormatPcmInt 32 -> mapAndLoad @Int32
        SampleFormatPcmInt 64 -> mapAndLoad @Int64
        _ -> error $ "The sample format \"" <> show (waveSampleFormat wave) <> "\" is not supported"
      where
        mapAndLoad :: forall a. (Real a, Num a, M.Storable a) => M.Vector M.D Pulse
        mapAndLoad =
          let rawArray =
                M.fromStorableVector @a M.Seq $
                  byteStringToVector sampleData
              (Max maxSample) = realToFrac <$> M.foldSemi (Max . abs) (Max 0) rawArray
           in M.map ((/ maxSample) . realToFrac) rawArray
        {-# INLINE mapAndLoad #-}

-- | Save the sound samples as raw floats
saveRaw :: FilePath -> M.Vector M.S Pulse -> IO ()
saveRaw filePath floats =
  B.writeFile filePath $ vectorToByteString $ M.toStorableVector floats

-- | Save the sound samples as raw floats compressed with gzip
saveRawCompressed :: FilePath -> M.Vector M.S Pulse -> IO ()
saveRawCompressed filePath floats = do
  let bytes = compress $ BL.fromStrict $ vectorToByteString $ M.toStorableVector floats
  BL.writeFile filePath bytes

-- Load the gzip compressed raw sound samples
loadRawCompressed :: FilePath -> IO (M.Vector M.S Pulse)
loadRawCompressed filePath = do
  file <- liftIO $ BL.readFile filePath
  pure $ M.fromStorableVector M.Seq $ byteStringToVector $ BL.toStrict $ decompress file

-- Load the raw sound samples
loadRaw :: FilePath -> IO (M.Vector M.S Pulse)
loadRaw filePath = do
  file <- liftIO $ B.readFile filePath
  pure $ M.fromStorableVector M.Seq $ byteStringToVector file
