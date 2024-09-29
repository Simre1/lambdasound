module LambdaSound.Convolution
  (  Kernel (..),
    convolveSamples,
    convolvePercentage,
    convolveDuration,
  )
where

import LambdaSound.Sound

import Data.Massiv.Array qualified as M
import Data.Coerce (coerce)

-- | A Kernel for convolution
data Kernel p = Kernel
  { coefficients :: p -> Float,
    size :: p,
    offset :: p
  }

convolve :: (Int -> Kernel Int) -> Sound d Pulse -> Sound d Pulse
convolve makeKernel = modifyWholeSound $ \wholeSound ->
  let (Kernel coefficients size offset) = makeKernel n
      n = M.unSz $ M.size wholeSound
      stencil = M.makeStencil (M.Sz1 size) offset $ \getV ->
        M.sum $ M.imap (\i -> (*) $ getV (i - offset)) computedCoefficients
      computedCoefficients =
        M.compute @M.S $
          if size <= 1
            then M.singleton 0.5
            else M.generate M.Seq (M.Sz1 size) $ \i ->
              coerce @_ @Pulse $ coefficients i
   in M.mapStencil M.Reflect stencil wholeSound

-- | Convolve a 'Sound' where the 'Kernel' size is
-- determined by 'Percentage's of the sound.
convolvePercentage :: Kernel Percentage -> Sound d Pulse -> Sound d Pulse
convolvePercentage (Kernel coefficients sizeP offsetP) = convolve $ \n ->
  let size = ceiling $ sizeP * fromIntegral n
   in Kernel
        { coefficients = \i -> coefficients (fromIntegral i / fromIntegral (size - 1)),
          size = size,
          offset = round $ offsetP * fromIntegral n
        }

-- | Convolve a 'Sound' where the 'Kernel' size is
-- determined by a 'Duration'.
convolveDuration :: Kernel Duration -> Sound T Pulse -> Sound T Pulse
convolveDuration (Kernel coefficients sizeD offsetD) sound@(TimedSound d _) =
  convolvePercentage
    (Kernel (coefficients . (* d) . coerce) (coerce $ sizeD / d) (coerce $ offsetD / d))
    sound

-- | Convolve a 'Sound' where the 'Kernel' size is
-- determined by the amount of samples. You have to keep in mind
-- that different sample rates will result in a different number of samples
-- for the same sound.
convolveSamples :: Kernel Int -> Sound T Pulse -> Sound T Pulse
convolveSamples kernel = convolve (const kernel)
