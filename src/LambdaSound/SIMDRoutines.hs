{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE ViewPatterns #-}

module LambdaSound.SIMDRoutines where

import Data.Vector.Storable.Mutable qualified as VSM
import Foreign
import Foreign.C

foreign import ccall safe "computeWave" computeWaveC :: Ptr MyFloat -> CInt -> CFloat -> Ptr Coefficients -> IO ()

computeWave :: VSM.IOVector MyFloat -> Float -> Coefficients -> IO ()
computeWave buffer dt coefficients = alloca $ \coefficientsPtr -> do
  poke coefficientsPtr coefficients
  VSM.unsafeWith buffer $ \bufferPtr ->
    computeWaveC bufferPtr (fromIntegral $ VSM.length buffer) (realToFrac dt) coefficientsPtr

newtype MyFloat = MyFloat CFloat deriving (Eq, Show, Ord, Num, Floating, Fractional, Real, RealFrac)

instance Storable MyFloat where
  sizeOf _ = sizeOf (undefined :: CFloat)
  alignment _ = 32
  peekElemOff ptr i = MyFloat <$> peekElemOff (castPtr ptr) i
  pokeElemOff ptr i (MyFloat cFloat) = pokeElemOff (castPtr ptr) i cFloat
  peekByteOff ptr i = MyFloat <$> peekByteOff (castPtr ptr) i
  pokeByteOff ptr i (MyFloat cFloat) = pokeByteOff (castPtr ptr) i cFloat
  peek ptr = MyFloat <$> peek (castPtr ptr)
  poke ptr (MyFloat cFloat) = poke (castPtr ptr) cFloat

data Coefficients = Coefficients
  { taylor0 :: Float,
    taylor1 :: Float,
    taylor2 :: Float,
    taylor3 :: Float,
    taylor4 :: Float,
    taylor5 :: Float,
    taylor6 :: Float,
    taylor7 :: Float,
    taylor8 :: Float,
    taylor9 :: Float,
    signum :: Float,
    abs :: Float
  }
  deriving (Eq, Show, Ord)

instance Storable Coefficients where
  sizeOf _ = sizeOf (undefined :: CFloat) * 12
  alignment _ = 32
  peek ptr =
    let fs = sizeOf @CFloat undefined
        ptr0 = castPtr ptr
        ptr1 = plusPtr ptr0 fs
        ptr2 = plusPtr ptr1 fs
        ptr3 = plusPtr ptr2 fs
        ptr4 = plusPtr ptr3 fs
        ptr5 = plusPtr ptr4 fs
        ptr6 = plusPtr ptr5 fs
        ptr7 = plusPtr ptr6 fs
        ptr8 = plusPtr ptr7 fs
        ptr9 = plusPtr ptr8 fs
        ptr10 = plusPtr ptr9 fs
        ptr11 = plusPtr ptr10 fs
     in Coefficients
          <$> peek ptr0
          <*> peek ptr1
          <*> peek ptr2
          <*> peek ptr3
          <*> peek ptr4
          <*> peek ptr5
          <*> peek ptr6
          <*> peek ptr7
          <*> peek ptr8
          <*> peek ptr9
          <*> peek ptr10
          <*> peek ptr11
  poke ptr (Coefficients a b c d e f g h i j k l) = do
    let fs = sizeOf @CFloat undefined
        ptr0 = castPtr ptr
        ptr1 = plusPtr ptr0 fs
        ptr2 = plusPtr ptr1 fs
        ptr3 = plusPtr ptr2 fs
        ptr4 = plusPtr ptr3 fs
        ptr5 = plusPtr ptr4 fs
        ptr6 = plusPtr ptr5 fs
        ptr7 = plusPtr ptr6 fs
        ptr8 = plusPtr ptr7 fs
        ptr9 = plusPtr ptr8 fs
        ptr10 = plusPtr ptr9 fs
        ptr11 = plusPtr ptr10 fs
    poke ptr0 a
    poke ptr1 b
    poke ptr2 c
    poke ptr3 d
    poke ptr4 e
    poke ptr5 f
    poke ptr6 g
    poke ptr7 h
    poke ptr8 i
    poke ptr9 j
    poke ptr10 k
    poke ptr11 l

coefficients0 :: Coefficients
coefficients0 = Coefficients 0 0 0 0 0 0 0 0 0 0 0 0

testCoefficients :: Coefficients -> [Float] -> [Float]
testCoefficients coefficients floats = applyCoefficients <$> floats
  where
    applyCoefficients x =
      coefficients.taylor0
        + coefficients.taylor1 * x
        + coefficients.taylor2 * x ^ 2
        + coefficients.taylor3 * x ^ 3
        + coefficients.taylor4 * x ^ 4
        + coefficients.taylor5 * x ^ 5
        + coefficients.taylor6 * x ^ 6
        + coefficients.taylor7 * x ^ 7
        + coefficients.taylor8 * x ^ 8
        + coefficients.taylor9 * x ^ 9
        + coefficients.signum * signum x
        + coefficients.abs * abs x

sineCoefficients :: Float -> Coefficients
sineCoefficients hz =
  coefficients0
    { taylor1 = hz * pi,
      taylor3 = -((hz * pi) ^ 3) / 6,
      taylor5 = (hz * pi) ^ 5 / 120,
      taylor7 = -((hz * pi) ^ 7 / 5040),
      taylor9 = (hz * pi) ^ 9 / 362880
    }

test :: [Float]
test = [-0.999818, -0.999795, -0.999773, -0.999750, -0.999727, -0.999705, -0.999682, -0.999659]

-- -0.000182 -0.000159 -0.000136 -0.000114 -0.000091 -0.000068 -0.000046 -0.000023
