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
  , getInfo :: EnvironmentInfo
  -- , getDroneList :: Ensemble.DroneList
  , getEnsemble :: Ensemble.EnsembleStatus
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
updateState nextActions (WorldState env view ensembleStatus) = (WorldState env view (updateEnsemble nextActions ensembleStatus))
  where updateEnsemble nextActions = stepEnsemble . (assignEnsemble nextActions)

--Apply the newly commanded actions to the ensembleStatus
--TODO: replace futile actions (e.g. Ascending when already high) with hover
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

--get new observations based on current ensembleStatus, then create a new WorlState from this information
--only need to run this when EnsembleStatus is such that one drone just completed a motion
observe :: WorldState -> EnvironmentInfo
observe wS = mergeEnvInfo (getInfo wS) newInfo
  where newInfo = envSnap (getEnv wS) (getEnsemble wS) :: EnvironmentInfo

mergeEnvInfo :: EnvironmentInfo -> EnvironmentInfo -> EnvironmentInfo
mergeEnvInfo = Map.unionWith takeBest

--make an ord instance instead? or maybe a Monoid type wapper like Sum?
takeBest :: PatchInfo -> PatchInfo -> PatchInfo
takeBest pi@(FullyObserved pat) _ = pi
takeBest _ pi@(FullyObserved pat) = pi
takeBest pi@(Classified detailReq) (Classified _) = pi
takeBest pi@(Classified detailReq) (Unseen) = pi
takeBest Unseen pi@(Classified detailReq) = PatchInfo
takeBest Unseen Unseen = Unseen

observePatch :: Altitude -> Patch -> PatchInfo
observePatch High (Patch detailReq) = Classified detailReq
observePatch Low patch = FullyObserved patch

addPatch :: Environment -> [(Position, Altitude)] -> [(Position, Altitude, Maybe Patch)]
addPatch _ [] = []
addPatch e@(Environment envMap) ((pos, alt) : posAlts) = (pos, alt, (Map.lookup pos envMap)) : (addPatch e posAlts)

swapMaybe :: (a, b, Maybe c) -> Maybe (a, b, c)
swapMaybe (_, _, Nothing) = Nothing
swapMaybe (a, b, Just c) = Just (a, b, c)

observeForChain :: (Position, Altitude, Patch) -> (Position, PatchInfo)
observeForChain (pos, alt, pat) = (pos, observePatch alt pat)

envSnap :: Environment -> EnsembleStatus -> EnvironmentInfo
envSnap env = Map.fromList . (fmap observeForChain) . catMaybes . (fmap swapMaybe) . (addPatch env) . minimalEnView

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