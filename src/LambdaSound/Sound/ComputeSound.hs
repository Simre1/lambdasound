module LambdaSound.Sound.ComputeSound where

import Data.Hashable
import Data.SomeStableName
import Foreign.Ptr
import GHC.Generics (Generic)
import LambdaSound.Sound.Types (Pulse)

newtype TimeDelta = TimeDelta Float

data ComputeSound a = ComputeSound
  { compute ::
      forall b.
      ((Ptr Pulse -> IO (Int -> IO a)) -> b) ->
      ((a ~ Pulse) => (Ptr Pulse -> Ptr Pulse -> IO ()) -> b) ->
      b,
    computeInfo :: ComputationInfo
  }

instance Eq (ComputeSound a) where
  (ComputeSound _ a) == (ComputeSound _ b) = a == b

instance Hashable (ComputeSound a) where
  hashWithSalt x (ComputeSound _ a) = hashWithSalt x a
  hash (ComputeSound _ a) = hash a

data ComputationInfo
  = ComputationInfoZip SomeStableName ComputationInfo ComputationInfo
  | ComputationInfoMap SomeStableName ComputationInfo
  | ComputationInfoPureIndexCompute SomeStableName
  | ComputationInfoSequentially ComputationInfo ComputationInfo
  | ComputationInfoParallel ComputationInfo ComputationInfo
  | ComputationInfoMakeIndexCompute SomeStableName
  | ComputationInfoModifyIndexCompute SomeStableName ComputationInfo
  | ComputationInfoModifyIndexCompute2 SomeStableName ComputationInfo ComputationInfo
  | ComputationInfoMapWholeComputation SomeStableName ComputationInfo
  | ComputationInfoChangeSampleRate SomeStableName ComputationInfo
  deriving (Eq, Generic)

instance Hashable ComputationInfo
