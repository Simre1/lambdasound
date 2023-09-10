module LambdaSound.Sound.Types where

import Control.DeepSeq (NFData)
import Data.Hashable (Hashable)
import Foreign.Storable (Storable)
import GHC.Generics (Generic)

-- | An audio sample
newtype Pulse = Pulse Float deriving (Show, Eq, Floating, Num, Fractional, Ord, Real, RealFrac, NFData, Storable, Hashable, Enum)

-- | The duration of a 'Sound'
newtype Duration = Duration Float deriving (Show, Eq, Floating, Num, Fractional, Ord, Real, RealFrac, NFData, Storable, Hashable, Enum)

-- | The progress of a 'Sound'. A sound progresses from '0' to '1'
-- while it plays.
newtype Progress = Progress Float deriving (Show, Eq, Floating, Num, Fractional, Ord, Real, RealFrac, NFData, Storable, Hashable, Enum)

-- | The percentage of a 'Sound'. '0.3' corresponds to 30% of a 'Sound'.
newtype Percentage = Percentage Float deriving (Show, Eq, Floating, Num, Fractional, Ord, Real, RealFrac, NFData, Storable, Hashable, Enum)

-- | Hz are the unit for frequencies. 440 Hz means that 440 oscillations happen per second
newtype Hz = Hz Float deriving (Show, Eq, Ord, Num, Fractional, Floating, Enum)

-- | Time progresses while a 'Sound' is playing and is used to create samples.
-- It is not guaranteed that 'Time' will correspond to the real runtime of a 'Sound' 
newtype Time = Time Float deriving (Show, Eq, Floating, Num, Fractional, Ord, Real, RealFrac, NFData, Storable, Hashable, Enum)

-- | Gives information about how many samples are needed during computation
data SamplingInfo = SamplingInfo
  { period :: !Float,
    samples :: !Int
  }
  deriving (Generic, Show, Eq)

instance Hashable SamplingInfo where
