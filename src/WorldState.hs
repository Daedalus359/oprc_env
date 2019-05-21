module WorldState where

import Drone
import Env
import EnvView
import MoveCosts
import Ensemble

import Data.Maybe
import Control.Monad --join
import qualified Data.Map as Map --unionWith

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
      show env, "\n",
      show view, "\n",
      show status, "\n"
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

observe :: EnsembleStatus -> WorldState -> WorldState
observe = undefined

--function from minimalEnView's [(Position, Altitude)] to [(Position, PatchInfo)]
--fromList to make that an EnvironmentInfo (Map.Map Position PatchInfo)
--create a preference function f :: PatchInfo -> PatchInfo -> PatchInfo
--use UnionWith to combine the existing and new EnvironmentInfos together

observePatch :: Altitude -> Patch -> PatchInfo
observePatch High (Patch detailReq) = Classified detailReq
observePatch Low patch = FullyObserved patch

patchMap :: [(Position, Altitude)] -> Environment -> [(Position, Altitude, Maybe Patch)]
patchMap [] _ = []
patchMap ((pos, alt) : posAlts) e@(Environment envMap) = (pos, alt, (Map.lookup pos envMap)) : (patchMap posAlts e)

swapMaybe :: (a, b, Maybe c) -> Maybe (a, b, c)
swapMaybe (_, _, Nothing) = Nothing
swapMaybe (a, b, Just c) = Just (a, b, c)

--returns a non-redundant 'map' of the best available views achievable given an ensemble status
--if each element in the list adds information compared to the rest of the list in either direction, then no elements are redundant
minimalEnView :: EnsembleStatus -> [(Position, Altitude)]
minimalEnView = nonImprovingViews . reverse . nonImprovingViews . ensembleView

--build a list from the back to front, adding new elements only when it improves upon the views in what comes later
nonImprovingViews :: [(Position, Altitude)] -> [(Position, Altitude)]
nonImprovingViews [] = []
nonImprovingViews ((pos, alt) : views)
  | newInfo = (pos, alt) : tailMap
  | otherwise = tailMap
    where tailMap = nonImprovingViews views
          newInfo = 
            case lookup pos tailMap of Nothing -> True --a view from any altitude is better than no information
                                       Just Low -> False --it can't get any better
                                       Just High -> (alt == Low) --add a low view if a high view exists

ensembleView :: EnsembleStatus -> [(Position, Altitude)]
ensembleView = join . (fmap viewList) . occupiedPositions

--list of the views available from one DronePosition
viewList :: DronePosition -> [(Position, Altitude)]
viewList dPos@(DronePos pos alt) = fmap (\p -> (p, alt)) $ viewableFrom dPos

--grabs the drone positions from an EnsembleStatus
occupiedPositions :: EnsembleStatus -> [DronePosition]
occupiedPositions [] = []
occupiedPositions ((_, (droneStat)) : ensStat) = position : (occupiedPositions ensStat)
  where position = case droneStat of (Unassigned pos) -> pos
                                     (Acting _ _ pos) -> pos
                                     (Assigned _ pos) -> pos

--lookup drone nextActions to get a Maybe Action
--turn this into a Maybe DroneStatus with fmap Assigned