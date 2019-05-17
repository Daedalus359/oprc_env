module WorldState where

import Drone
import Env
import EnvView
import MoveCosts
import Ensemble

import Data.Maybe

--everything, including a description of the partial infomation available to an agent
data WorldState =
  WorldState {
    getEnv :: Env.Environment
  , getView :: EnvironmentInfo
  , getDroneList :: Ensemble.DroneList
  , getEnsembleStatus :: Ensemble.EnsembleStatus
  }
  deriving Eq

--TODO: make a smart constructor for WorldState that checks everything for consistency (e.g. between droneList and ensembleStatus)

type NextActions = [(Drone, Action)]

--update ensemble status based next actions
--make observations, update view based on that
--make it return type (WorldState, WorldView) eventually
updateState :: WorldState -> NextActions -> WorldState
updateState (WorldState env view droneList ensembleStatus) nextActions = (WorldState env view droneList (updateEnStatus ensembleStatus nextActions))

--should I give WorldState a functor instance so I can update EnStatus using fmap?

--change this to not update a DroneStatus of Acting action
updateEnStatus :: EnsembleStatus -> NextActions -> EnsembleStatus
updateEnStatus (ds@(drone, Acting action stepsRem) : ensStat) nextActions = ds : (updateEnStatus ensStat nextActions)
updateEnStatus ((drone, droneStat) : ensStat) nextActions =
  (drone, fromMaybe droneStat $ fmap Assigned $ lookup drone nextActions) : (updateEnStatus ensStat nextActions)

--lookup drone nextActions to get a Maybe Action
--turn this into a Maybe DroneStatus with fmap Assigned
