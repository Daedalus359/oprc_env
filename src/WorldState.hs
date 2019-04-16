module WorldState where

import Drone
import Env
import EnvView
import MoveCosts
import Ensemble

import Data.Maybe

data WorldState =
  WorldState {
    env :: Env.Environment
  , view :: EnvView.EnvironmentInfo
  , droneList :: DroneList
  , ensembleStatus :: EnsembleStatus
    }

type NextActions = [(DroneID, Action)]

--update ensemble status based next actions
--make observations, update view based on that
updateWorld :: WorldState -> NextActions -> WorldState
updateWorld (WorldState env view droneList ensembleStatus) nextActions = (WorldState env view droneList (updateEnStatus ensembleStatus nextActions))

--should I give WorldState a functor instance so I can update EnStatus using fmap?

--change this to not update a DroneStatus of Acting action
updateEnStatus :: EnsembleStatus -> NextActions -> EnsembleStatus
updateEnStatus (ds@(droneID, Acting action stepsRem) : ensStat) nextActions = ds : (updateEnStatus ensStat nextActions)
updateEnStatus ((droneID, droneStat) : ensStat) nextActions =
  (droneID, fromMaybe droneStat $ fmap Assigned $ lookup droneID nextActions) : (updateEnStatus ensStat nextActions)

--lookup droneID nextActions to get a Maybe Action
--turn this into a Maybe DroneStatus with fmap Assigned