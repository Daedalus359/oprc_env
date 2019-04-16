module EnvView where --a representation of (possibly incomplete) knowledge about the Environment

import Env
import Drone
import qualified Data.Map as Map

--given a patch of land, what do we know about it?
data PatchInfo =
    Unseen --nothing is known about the patch
  | Classified DetailReq --only the type of the patch is known
  | FullyObserved Env.Patch --this patch has been adequately observed

--the list of all positions which can be seen from a given position and altitude
viewableFrom :: DronePosition -> [Position]
viewableFrom (DronePos pos Low) = [pos]
viewableFrom (DronePos pos High) = pos : (Env.neighborsOf pos)

type EnvironmentInfo = Map.Map Position PatchInfo
