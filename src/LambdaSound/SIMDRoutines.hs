{-# LANGUAGE ForeignFunctionInterface #-}

module LambdaSound.SIMDRoutines where

import Foreign
import Foreign.C

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

foreign import ccall safe "simple_sin" simpleSin :: Ptr MyFloat -> CInt -> CFloat -> CFloat -> IO ()

foreign import ccall safe "setup_simd_routines" setupSIMDRoutines :: IO ()

data Coefficients = Coefficients
  { a1 :: Float,
    a2 :: Float,
    a3 :: Float,
    a4 :: Float,
    a5 :: Float,
    a6 :: Float,
    a7 :: Float,
    a8 :: Float,
    a9 :: Float,
    a10 :: Float,
    a11 :: Float
  }
