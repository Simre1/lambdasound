module LambdaSound.Sound.ComputeSound where

import Data.Hashable
import Data.SomeStableName
import Foreign.Ptr
import GHC.Generics (Generic)
import LambdaSound.Sound.Types (Pulse)

data ComputeSound a = ComputeSound
  { compute ::
      forall b.
      ((Ptr Pulse -> IO (Int -> IO a)) -> b) ->
      ((a ~ Pulse) => (Ptr Pulse -> Ptr Pulse -> IO ()) -> b) ->
      b,
    computeInfo :: ComputationInfo
  }

data ComputationInfo
  = ComputationInfoZip SomeStableName ComputationInfo ComputationInfo
  | ComputationInfoMap SomeStableName ComputationInfo
  | ComputationInfoPureIndexCompute SomeStableName
  | ComputationInfoSequentially ComputationInfo ComputationInfo
  | ComputationInfoParallel ComputationInfo ComputationInfo
  | ComputationInfoMakeIndexCompute SomeStableName
  | ComputationInfoModifyIndexCompute SomeStableName SomeStableName ComputationInfo
  | ComputationInfoMapWholeComputation SomeStableName ComputationInfo
  deriving (Eq, Generic)

instance Hashable ComputationInfo

