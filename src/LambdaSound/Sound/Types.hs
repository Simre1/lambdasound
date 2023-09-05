module LambdaSound.Sound.Types where

import Control.DeepSeq (NFData)
import Data.Hashable (Hashable)
import Foreign.Storable (Storable)
import GHC.Generics (Generic)

-- | Gives information about how many samples are needed during computation
data SampleRate = SampleRate
  { period :: !Float,
    samples :: !Int
  }
  deriving (Show, Generic, Eq)

instance Hashable SampleRate

-- | An audio sample
newtype Pulse = Pulse Float deriving (Show, Eq, Floating, Num, Fractional, Ord, Real, RealFrac, NFData, Storable, Hashable, Enum)

-- | The duration of a 'Sound'
newtype Duration = Duration Float deriving (Show, Eq, Floating, Num, Fractional, Ord, Real, RealFrac, NFData, Storable, Hashable, Enum)

-- | The progress of a 'Sound'. A sound progresses from '0' to '1'
-- while it plays.
newtype Progress = Progress Float deriving (Show, Eq, Floating, Num, Fractional, Ord, Real, RealFrac, NFData, Storable, Hashable, Enum)

-- | The percentage of a 'Sound'. '0.3' corresponds to 30% of a 'Sound'.
newtype Percentage = Percentage Float deriving (Show, Eq, Floating, Num, Fractional, Ord, Real, RealFrac, NFData, Storable, Hashable, Enum)

newtype Hz = Hz Float deriving (Show, Eq, Ord, Num, Fractional, Floating, Enum)
