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
  -- , getDroneList :: Ensemble.DroneList
  , getEnsembleStatus :: Ensemble.EnsembleStatus
  }
  deriving (Eq, Show)

--TODO: make a smart constructor for WorldState that checks everything for consistency (e.g. between droneList and ensembleStatus)

type NextActions = [(Drone, Action)]

--update ensemble status based next actions
--make observations, update view based on that
--make it return type (WorldState, WorldView) eventually
updateState :: WorldState -> NextActions -> WorldState
updateState (WorldState env view ensembleStatus) nextActions = (WorldState env view (assignEnsemble nextActions ensembleStatus))

--change this to not update a DroneStatus of Acting action
assignEnsemble :: NextActions -> EnsembleStatus -> EnsembleStatus
assignEnsemble nextActions ((drone, droneStat@(Unassigned pos)) : ensStat) =
  (drone, newStatus) : (assignEnsemble nextActions ensStat)
    where newStatus = (fromMaybe droneStat $ fmap toAssigned (lookup drone nextActions))
          toAssigned = (\a -> Assigned a pos)
assignEnsemble nextActions (ds : ensStat) = ds : (assignEnsemble nextActions ensStat)--ignore new commands for drones alreacy acting or assigned

stepEnsemble :: EnsembleStatus -> EnsembleStatus
stepEnsemble = undefined

observe :: EnsembleStatus -> EnvironmentInfo -> EnvironmentInfo
observe = undefined

--lookup drone nextActions to get a Maybe Action
--turn this into a Maybe DroneStatus with fmap Assigned
