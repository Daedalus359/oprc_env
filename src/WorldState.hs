module WorldState where

import Drone
import Env
import EnvView
import MoveCosts

data WorldState =
  WorldState
    Env.Environment
    EnvView.EnvironmentInfo
                    
