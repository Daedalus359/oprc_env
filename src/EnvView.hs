module EnvView where --a representation of (possibly incomplete) knowledge about the Environment

import Env
import Drone
import Ensemble
import qualified Data.Map.Strict as Map

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

toFootprint :: EnvironmentInfo -> Footprint
toFootprint = Map.keysSet

--ideally find a way to quickly swap in the Unseen value as the value to every key
initializeInfo :: Environment -> EnvironmentInfo
initializeInfo env = fmap (const Unseen) $ getMap env

data WorldView = 
  WorldView {
    getView :: EnvironmentInfo
  -- , getDroneList :: Ensemble.DroneList
  , getEnsembleStatus :: Ensemble.EnsembleStatus
  }
  deriving Eq

numDronesRunning :: WorldView -> Int
numDronesRunning (WorldView _ enStat) = length enStat 