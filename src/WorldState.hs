module WorldState where

import Drone
import Env
import EnvView
import MoveCosts
import Ensemble

import Data.Maybe
import qualified Data.Map as Map
import qualified Data.Set as Set

--everything, including a description of the partial infomation available to an agent
data WorldState =
  WorldState {
    getEnv :: Env.Environment
  , getView :: EnvironmentInfo
  -- , getDroneList :: Ensemble.DroneList
  , getEnsembleStatus :: Ensemble.EnsembleStatus
  }
  deriving Eq

instance Show WorldState where
  show (WorldState env view status) = concat 
    ["WorldState: \n",
      "\t", show env,
      "\t", show view,
      "\t", show status
    ]

--TODO: make a smart constructor for WorldState that checks everything for consistency (e.g. between droneList and ensembleStatus)

type NextActions = [(Drone, Action)]

--update ensemble status based next actions
--make observations, update view based on that
--make it return type (WorldState, WorldView) eventually
updateState :: NextActions -> WorldState -> WorldState
updateState nextActions (WorldState env view ensembleStatus) = (WorldState env view (assignEnsemble nextActions ensembleStatus))

--replace futile actions (e.g. Ascending when already high) with hover
assignEnsemble :: NextActions -> EnsembleStatus -> EnsembleStatus
assignEnsemble _ [] = []
assignEnsemble nextActions ((drone, droneStat@(Unassigned pos)) : ensStat) =
  (drone, newStatus) : (assignEnsemble nextActions ensStat)
    where newStatus = (fromMaybe droneStat $ fmap toAssigned (lookup drone nextActions))
          toAssigned = (\a -> Assigned a pos)
assignEnsemble nextActions (ds : ensStat) = ds : (assignEnsemble nextActions ensStat)--ignore new commands for drones alreacy acting or assigned

stepEnsemble :: EnsembleStatus -> EnsembleStatus
stepEnsemble [] = []
stepEnsemble ((drone, stat@(Unassigned _)) : enStat) = (drone, stat) : (stepEnsemble enStat)
stepEnsemble ((drone, (Acting action steps pos)) : enStat)
  | steps > 1 = (drone, (Acting action (steps - 1) pos)) : (stepEnsemble enStat)
  | steps <= 1 = (drone, (Unassigned newPos)) : (stepEnsemble enStat)
    where newPos = movedBy action pos
stepEnsemble ((drone, (Assigned action pos)) : enStat) = (drone, (Acting action steps pos)) : (stepEnsemble enStat)
  where steps = duration action

observe :: EnsembleStatus -> EnvironmentInfo -> EnvironmentInfo
observe = undefined

--returns a map from positions to the altitude of the lowest drone that can view it
viewableMap :: EnsembleStatus -> Map.Map Position Altitude
viewableMap = undefined

occupiedPositions :: EnsembleStatus -> [DronePosition]
occupiedPositions [] = []
occupiedPositions ((_, (droneStat)) : ensStat) = position : (occupiedPositions ensStat)
  where position = case droneStat of (Unassigned pos) -> pos
                                     (Acting _ _ pos) -> pos
                                     (Assigned _ pos) -> pos


--lookup drone nextActions to get a Maybe Action
--turn this into a Maybe DroneStatus with fmap Assigned
