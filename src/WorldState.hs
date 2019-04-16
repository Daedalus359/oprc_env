module WorldState where

import Drone
import Env
import EnvView
import MoveCosts
import Ensemble

data WorldState =
  WorldState {
    env :: Env.Environment
  , view :: EnvView.EnvironmentInfo
  , droneList :: DroneList
  , ensembleStatus :: EnsembleStatus
    }
