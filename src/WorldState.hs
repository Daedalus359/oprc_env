module WorldState where

import Drone
import Env
import EnvView
import MoveCosts
import Ensemble

import Data.Maybe
import Control.Monad --join
import qualified Data.Map.Strict as Map --unionWith
import qualified Data.Set as Set
import Data.Monoid
import System.Random

--everything, including a description of the partial infomation available to an agent
data WorldState =
  WorldState {
    getEnv :: Env.Environment
  , getInfo :: EnvironmentInfo
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
updateState :: NextActions -> WorldState -> WorldState
updateState nextActions ws@(WorldState env view ensembleStatus) = (WorldState env (observe $ WorldState env view newStatus)) newStatus
  where
    updateEnsemble nextActions = (stepEnsemble $ Map.keysSet $ toMap env). (assignEnsemble nextActions)
    newStatus = (updateEnsemble nextActions ensembleStatus)

--a version of updateState in which drone dropout can occur
updateStateDrop :: Float -> NextActions -> (StdGen, WorldState) -> (StdGen, WorldState)
updateStateDrop dropoutProb nextActions (gen, ws@(WorldState env view ensembleStatus)) = undefined

toView :: WorldState -> WorldView
toView (WorldState env info status) = WorldView info status

--Applies the newly commanded actions to the ensembleStatus
--TODO: replace futile actions (e.g. Ascending when already high) with hover
assignEnsemble :: NextActions -> EnsembleStatus -> EnsembleStatus
assignEnsemble _ [] = []
--when a drone is unassigned, check to see if we can 
assignEnsemble nextActions ((drone, droneStat@(Unassigned pos)) : ensStat) =
  (drone, newStatus) : (assignEnsemble nextActions ensStat)
    where newStatus = (fromMaybe droneStat $ fmap toAssigned (lookup drone nextActions))
          toAssigned = (\a -> Assigned a pos)
--when a drone has a status other than Unassigned, it just continues on its commanded action
assignEnsemble nextActions (ds : ensStat) = ds : (assignEnsemble nextActions ensStat)

--Advances each drone 1 time step according to its current status. Should run right after assignEnsemble.
stepEnsemble :: Footprint -> EnsembleStatus -> EnsembleStatus
stepEnsemble fp [] = []--recursive base case
--unassigned drones contine to be unassigned in the next time step
stepEnsemble fp ((drone, stat@(Unassigned _)) : enStat) = (drone, stat) : (stepEnsemble fp enStat)
--acting drones take a step towards completing their actions
stepEnsemble fp ((drone, (Acting action steps pos)) : enStat)
  | steps > 1 = (drone, (Acting action (steps - 1) pos)) : (stepEnsemble fp enStat)
  | steps <= 1 = (drone, (Unassigned newPos)) : (stepEnsemble fp enStat)--complete the action, causing the drone to be unassigned and possibly in a new position
    where newPos = movedBy action pos
--drones which have just received a new assignment are set in motion
stepEnsemble fp ((drone, (Assigned action pos)) : enStat) =
  case (Drone.validMove fp pos action && stepsRemaining >= 1) of
    True -> (drone, (Acting action stepsRemaining pos)) : (stepEnsemble fp enStat)
    False -> (drone, (Unassigned pos)) : (stepEnsemble fp enStat)
  
  -- (- 1) because you have just completed the first step, since this is the step function
  where stepsRemaining = duration action - 1

--Combines EnvironmentInfo contained in current WorldState with new EnvironmentInfo collected from envSnap
--only need to run this when EnsembleStatus is such that one drone just completed a motion
observe :: WorldState -> EnvironmentInfo
observe wS = mergeEnvInfo (getInfo wS) newInfo
  where newInfo = envSnap (getEnv wS) (getEnsemble wS) :: EnvironmentInfo

--Combines two EnvironmentInfos according to which one contains more complete info about each location
--meant to be run with two accurate EnvironmentInfos that describe the same Environment
mergeEnvInfo :: EnvironmentInfo -> EnvironmentInfo -> EnvironmentInfo
mergeEnvInfo = Map.unionWith takeBest

--Compares two PatchInfos and returns the one with more complete info.
--Prefers the first PatchInfo supplied in case they contain equal observation levels
takeBest :: PatchInfo -> PatchInfo -> PatchInfo
takeBest pi@(FullyObserved pat) _ = pi
takeBest _ pi@(FullyObserved pat) = pi
takeBest pi@(Classified detailReq) (Classified _) = pi
takeBest pi@(Classified detailReq) (Unseen) = pi
takeBest Unseen pi@(Classified detailReq) = pi
takeBest Unseen Unseen = Unseen

--function that gives the info that results from viewing the supplied patch from the supplied altitude
observePatch :: Altitude -> Patch -> PatchInfo
observePatch Low patch = FullyObserved patch
observePatch High p@(Patch Far) = FullyObserved p
observePatch High (Patch Close) = Classified Close

--augments a list of positions and altitude with the Just a Patch that is below that position, if one exists in the provided Environment
addPatch :: Environment -> [(Position, Altitude)] -> [(Position, Altitude, Maybe Patch)]
addPatch _ [] = []
addPatch e@(Environment envMap) ((pos, alt) : posAlts) = (pos, alt, (Map.lookup pos envMap)) : (addPatch e posAlts)

swapMaybe :: (a, b, Maybe c) -> Maybe (a, b, c)
swapMaybe (_, _, Nothing) = Nothing
swapMaybe (a, b, Just c) = Just (a, b, c)

--goes from the information required to specify an observation to the observation itself
observeForChain :: (Position, Altitude, Patch) -> (Position, PatchInfo)
observeForChain (pos, alt, pat) = (pos, observePatch alt pat)

--
envSnap :: Environment -> EnsembleStatus -> EnvironmentInfo
envSnap env = Map.fromList . (fmap observeForChain) . catMaybes . (fmap swapMaybe) . (addPatch env) . minimalEnView--catMaybes :: [Maybe a] -> [a]

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