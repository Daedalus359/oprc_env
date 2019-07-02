module EnvView where --a representation of (possibly incomplete) knowledge about the Environment

import Env
import Drone
import Ensemble
import qualified Data.Map as Map

--given a patch of land, what do we know about it?
data PatchInfo =
    Unseen --nothing is known about the patch
  | Classified DetailReq --only the type of the patch is known
  | FullyObserved Env.Patch --this patch has been adequately observed
  deriving (Eq, Show)

isFullyObserved :: PatchInfo -> Bool
isFullyObserved (FullyObserved _) = True
isFullyObserved _ = False

type EnvironmentInfo = Map.Map Position PatchInfo

data WorldView = 
  WorldView {
    getView :: EnvironmentInfo
  -- , getDroneList :: Ensemble.DroneList
  , getEnsembleStatus :: Ensemble.EnsembleStatus
  }
  deriving Eq