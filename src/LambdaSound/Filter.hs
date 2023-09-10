-- | This module implements IIR filter.
--
-- See: http://shepazu.github.io/Audio-EQ-Cookbook/audio-eq-cookbook.html
module LambdaSound.Filter
  ( -- * Usage
    IIRParams,
    applyIIRFilter,

    -- * Design
    lowPassFilter,
    highPassFilter,
    bandPassFilter,
  )
where

import Control.Monad.ST
import Data.Coerce (coerce)
import Data.Massiv.Array qualified as M
import Data.STRef.Strict
import LambdaSound.Sound

data IIRParams = IIRParams
  { b0 :: {-# UNPACK #-} !Float,
    b1 :: {-# UNPACK #-} !Float,
    b2 :: {-# UNPACK #-} !Float,
    a0 :: {-# UNPACK #-} !Float,
    a1 :: {-# UNPACK #-} !Float,
    a2 :: {-# UNPACK #-} !Float
  }
  deriving (Show)

-- | A low-pass filter using cutoff frequency and resonance.
lowPassFilter :: Hz -> Float -> SamplingInfo -> IIRParams
lowPassFilter freq q si =
  IIRParams
    { b0,
      b1 = 1 - cos w0,
      b2 = b0,
      a0 = 1 + a,
      a1 = -2 * cos w0,
      a2 = 1 - a
    }
  where
    b0 = (1 - cos w0) / 2
    w0 = calcW0 si.sampleRate freq
    a = calcAQ w0 q

highPassFilter :: Hz -> Float -> SamplingInfo -> IIRParams
highPassFilter freq q si =
  IIRParams
    { b0,
      b1 = -1 * (1 + cos w0),
      b2 = b0,
      a0 = 1 + a,
      a1 = -2 * cos w0,
      a2 = 1 - a
    }
  where
    b0 = (1 + cos w0) / 2
    w0 = calcW0 si.sampleRate freq
    a = calcAQ w0 q

bandPassFilter :: Hz -> Float -> SamplingInfo -> IIRParams
bandPassFilter freq q si =
  IIRParams
    { b0 = a,
      b1 = 0,
      b2 = -1 * a,
      a0 = 1 + a,
      a1 = -2 * cos w0,
      a2 = 1 - a
    }
  where
    w0 = calcW0 si.sampleRate freq
    a = calcAQ w0 q

calcW0 :: Hz -> Hz -> Float
calcW0 sampleRate freq = coerce $ 2 * pi * freq / sampleRate

calcAQ :: Float -> Float -> Float
calcAQ w0 q = sin w0 / (2 * q)

data IIRState = IIRState
  { x0 :: {-# UNPACK #-} !Float,
    x1 :: {-# UNPACK #-} !Float,
    x2 :: {-# UNPACK #-} !Float,
    y0 :: {-# UNPACK #-} !Float,
    y1 :: {-# UNPACK #-} !Float,
    y2 :: {-# UNPACK #-} !Float
  }
  deriving (Show)

applyIIRFilter :: (SamplingInfo -> IIRParams) -> Sound d Pulse -> Sound d Pulse
applyIIRFilter makeParams sound =
  case sound of
    TimedSound d _ -> withSamplingInfoT d $ \si -> applyFilter (makeParams si) sound
    InfiniteSound _ -> withSamplingInfoI $ \si -> applyFilter (makeParams si) sound
  where
    applyFilter :: IIRParams -> Sound d Pulse -> Sound d Pulse
    applyFilter params =
      modifyWholeSound
        ( \source -> runST $ do
            stateRef <- newSTRef initialIIRState
            M.traversePrim @M.S (processSample stateRef) source
        )
      where
        processSample :: STRef s IIRState -> Pulse -> ST s Pulse
        processSample stateRef (Pulse sample) = do
          iirState <- readSTRef stateRef
          let newState = applyIIR params sample iirState
          writeSTRef stateRef newState
          pure $ Pulse newState.y0
        initialIIRState :: IIRState
        initialIIRState = IIRState 0 0 0 0 0 0

        applyIIR :: IIRParams -> Float -> IIRState -> IIRState
        applyIIR (IIRParams b0 b1 b2 a0 a1 a2) x0 (IIRState x1 x2 _ y1 y2 _) = newState
          where
            newState = IIRState x0 x1 x2 newSample y1 y2
            newSample = (b0 / a0) * x0 + (b1 / a0) * x1 + (b2 / a0) * x2 - (a1 / a0) * y1 - (a2 / a0) * y2
