module HierarchicalPolicy where

import AgentUtils
import Drone
import Env
import EnvView
import GraphOPRC
import Policy
import SpanningLoop
import SpanningTreeAgent

import qualified Data.Map as Map
import Data.Monoid
import qualified Data.Set as Set
import qualified Data.Sequence as SQ
import System.Random

data HighFirstBFSPolicy = HighFirstBFSPolicy AltitudePhase StdGen (Map.Map DTPath Footprint)

instance Policy HighFirstBFSPolicy where
  nextMove pol@(HighFirstBFSPolicy LowSweep _ _) wv = --in this case, just do whatever the AdaptiveBFSPolicy does and repackage
    (nextMoves, repackagedPol)
    where
      repackagedPol = fromALBP nextLowPol
      (nextMoves, nextLowPol) = nextMove (toALBP pol) wv

  nextMove (HighFirstBFSPolicy HighSweep gen map) wv@(WorldView envInfo enStat) =  --the real novelty of this policy
    if (not anythingToDo)
      then nextMove (HighFirstBFSPolicy LowSweep gen kMeansResetMap) wv
      else (nas, HighFirstBFSPolicy HighSweep newPolicyGen newMap)
    where
      (nas, newMap) = Map.foldrWithKey (accumNextActionsAndMap High wv) ([], Map.empty) $ Map.mapKeys (removeObservedWaypoints toDo) actionableMap

      actionableMap =
        if anyDroneNeedsTerritory
          then
            Map.foldrWithKey (refreshWaypoints highSmartEdgeBFSCoarsePath enStat boundsSet) Map.empty $
              fmap (detailedSetFromNodeCorners 6 toDo) $ kmiByFootprint kmGen (coarseMap 6 toDo) 4 filteredWaypointsMap
          else filteredWaypointsMap

      anyDroneNeedsTerritory = getAny $ foldMap (Any . (dtpNeedsNewMoves enStat)) $ Map.keysSet filteredWaypointsMap

      --currently this does nothing because filtering waypoints is the wrong approach. 
      filteredWaypointsMap = Map.mapKeys (removeObservedWaypoints toDo) currentDronesMap

      currentDronesMap = Map.filterWithKey (droneInSet aliveDrones) map
      aliveDrones = Set.fromList $ fmap fst enStat

      
      boundsSet = toFootprint envInfo
      (kmGen, newPolicyGen) = split gen

      anythingToDo = not $ Set.null toDo
      toDo = unseenLocations envInfo --what is still worth vising from the standpoint of a high policy
      blankSlate (DTPath dt _ _) = DTPath dt [] []
      blankMap = Map.fromSet (const Set.empty) $ Set.map blankSlate $ Map.keysSet map
      kMeansResetMap = fmap (detailedSetFromQuadCenters 2 (incompleteLocations envInfo)) $ kMeansInternal ((coarseQuadrantCenters 2) . incompleteLocations) kmGen envInfo 10 blankMap--ok to use kmGen in two places because only one gets run


removeObservedWaypoints2 :: HasWaypoints w => EnvironmentInfo -> w -> w
removeObservedWaypoints2 envInfo w = setWP w $ dropWhile (not . (highWaypointQuadrantHasUnobserved2 envInfo)) $ getWP w

removeObservedWaypoints :: HasWaypoints w => Footprint -> w -> w
removeObservedWaypoints unseenLocs w = setWP w $ dropWhile (not . (highWaypointQuadrantHasUnobserved unseenLocs)) $ getWP w

highWaypointQuadrantHasUnobserved2 :: EnvironmentInfo -> Position -> Bool
highWaypointQuadrantHasUnobserved2 envInfo quadCenter =
  getAny $ foldMap (Any . (\pos -> Set.member pos (unseenLocations envInfo))) quadSet
  where
    quadSet = quadSetFromCenter 3 quadCenter

highWaypointQuadrantHasUnobserved :: Footprint -> Position -> Bool
highWaypointQuadrantHasUnobserved unseenLocs quadCenter =
  getAny $ foldMap (Any . (\pos -> Set.member pos unseenLocs)) quadSet
  where
    quadSet = quadSetFromCenter 3 quadCenter

--the HighFirstBFSPolicy that is functionally identical to the given ALBP
fromALBP :: AdaptiveLowBFSPolicy -> HighFirstBFSPolicy
fromALBP (AdaptiveLowBFSPolicy gen map) = HighFirstBFSPolicy LowSweep gen map

--this will ignore the altitude phase, and then fromALBP will always return one in low mode
toALBP :: HighFirstBFSPolicy -> AdaptiveLowBFSPolicy
toALBP (HighFirstBFSPolicy _ gen map) = AdaptiveLowBFSPolicy gen map

initializeHFBFSP :: Int -> StdGen -> WorldView -> HighFirstBFSPolicy
initializeHFBFSP iterations gen wv@(WorldView envInfo enStat) = HighFirstBFSPolicy HighSweep polGen $ fmap (detailedSetFromQuadCenters 3 (incompleteLocations envInfo)) $ kMeansAlt High iterations kmGen envInfo dtpSeq
  where
    dtpSeq = SQ.fromList $ fmap (\(drone, pos) -> DTPath (DroneTerritory drone pos) [] []) $ fmap (fmap groundPos) enStat
    (kmGen, polGen) = split gen

--useless except for testing purposes, functionally identical to the AdaptiveLowBFSPolicy version
initializeLowImmediately :: Int -> StdGen -> WorldView -> HighFirstBFSPolicy
initializeLowImmediately i g w = fromALBP $ initializeALBP i g w