module LambdaSound.Sound where

newtype Percent = Percent Float

newtype Progress = Progress Float

data BasicArithmetic a
  = Add a a
  | Multiply a a
  | Subtract a a
  | Divide a a
  | Abs a
  | Signum a

data Kernel = Kernel
  { distribution :: Distribution,
    size :: Percent
  }

data Distribution
  = NormalDistribution
      { mean :: Float,
        standardDeviation :: Float
      }
  | UniformDistribution {height :: Float}
  | CombineDistribution {operation :: BasicArithmetic Distribution}
  | TranslateDistribution {translation :: Float, distribution :: Distribution}

data ElementarySound
  = SineWave {amplitude :: Float, hz :: Float}
  | ConstantValue {value :: Float}

data Sound
  = ElementarySound ElementarySound
  | Sequence {seperator :: Percent, sound1 :: Sound, sound2 :: Sound}
  | Convolution {kernel :: Kernel}
  | Elementwise {operation :: BasicArithmetic Sound}

instance Num Sound where
  s1 + s2 = Elementwise $ Add s1 s2
  s1 - s2 = Elementwise $ Subtract s1 s2
  s1 * s2 = Elementwise $ Multiply s1 s2
  abs s = Elementwise $ Abs s
  signum s = Elementwise $ Signum s
  fromInteger v = ElementarySound $ ConstantValue $ fromInteger v
  negate s = 0 - s

instance Fractional Sound where
  fromRational v = ElementarySound $ ConstantValue $ fromRational v
  s1 / s2 = Elementwise $ Divide s1 s2
  recip v = 1 / v
