-- | This module implements IIR filters.
--
-- See: http://shepazu.github.io/Audio-EQ-Cookbook/audio-eq-cookbook.html
module LambdaSound.Filter
  ( -- * Usage
    IIRParams (..),
    applyIIRFilter,

    -- * Design
    lowPassFilter,
    highPassFilter,
    bandPassFilter,
  )
where

import Control.Monad (forM_)
import Data.Coerce (coerce)
import Data.Massiv.Array qualified as M
import Data.Massiv.Array.Unsafe qualified as MU
import Data.Maybe (fromMaybe)
import LambdaSound.Sound

-- | IIRParams contains the filter coefficients for the forward and
-- feedback computation
data IIRParams = IIRParams
  { feedforward :: !(M.Vector M.S Float),
    feedback :: !(M.Vector M.S Float)
  }
  deriving (Show)

-- | A low-pass filter using cutoff frequency and resonance.
lowPassFilter :: Hz -> Float -> SamplingInfo -> IIRParams
lowPassFilter freq q si =
  IIRParams (M.fromList M.Seq [b0, 1 - cos w0, b0]) (M.fromList M.Seq [1 + a, -2 * cos w0, 1 - a])
  where
    b0 = (1 - cos w0) / 2
    w0 = calcW0 si.sampleRate scaledFreq
    a = calcAQ w0 q
    scaledFreq = freq / (si.sampleRate * coerce (si.period))

-- | A high-pass filter using cutoff frequency and resonance.
highPassFilter :: Hz -> Float -> SamplingInfo -> IIRParams
highPassFilter freq q si =
  IIRParams (M.fromList M.Seq [b0, -1 - cos w0, b0]) (M.fromList M.Seq [1 + a, -2 * cos w0, 1 - a])
  where
    b0 = (1 + cos w0) / 2
    w0 = calcW0 si.sampleRate scaledFreq
    a = calcAQ w0 q
    scaledFreq = freq / (si.sampleRate * coerce (si.period))

-- | A band pass filter using cutoff frequency and resonance.
bandPassFilter :: Hz -> Float -> SamplingInfo -> IIRParams
bandPassFilter freq q si =
  IIRParams (M.fromList M.Seq [a, 0, -1 * a]) (M.fromList M.Seq [1 + a, -2 * cos w0, 1 - a])
  where
    w0 = calcW0 si.sampleRate scaledFreq
    a = calcAQ w0 q
    scaledFreq = freq / (si.sampleRate * coerce (si.period))

calcW0 :: Hz -> Hz -> Float
calcW0 sampleRate freq = coerce $ 2 * pi * freq / sampleRate

calcAQ :: Float -> Float -> Float
calcAQ _ 0 = 0
calcAQ w0 q = sin w0 / (2 * q)

applyIIRFilter :: (SamplingInfo -> IIRParams) -> Sound d Pulse -> Sound d Pulse
applyIIRFilter makeParams sound = adoptDuration sound $ withSamplingInfo $ \si ->
  applyFilter (makeParams si) sound
  where
    applyFilter :: IIRParams -> Sound d Pulse -> Sound d Pulse
    applyFilter (IIRParams feedforward feedback') =
      let (currentCoefficient, feedback) = (coerce $ M.defaultIndex 1 feedback' 0, M.tail feedback')
       in modifyWholeSoundST $ \source dest -> do
            forM_ [0 .. pred (M.unSz $ M.sizeOfMArray dest)] $ \index -> do
              let sourceValues = M.imap (\i v -> coerce v * M.defaultIndex 0 source (index - i)) feedforward
              recursiveValues <- M.itraversePrim @M.S (\i v -> (coerce v *) . fromMaybe 0 <$> M.read dest (index - succ i)) feedback

              let currentValue = (M.sum sourceValues - M.sum recursiveValues) / currentCoefficient
              MU.unsafeWrite dest index currentValue
