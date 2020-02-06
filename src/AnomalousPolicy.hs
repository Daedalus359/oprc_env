module AnomalousPolicy where

import AgentUtils
import Drone
import Env
import EnvView
import GraphOPRC
import Policy
import SpanningLoop
import SpanningTreeAgent

import HierarchicalPolicy

import qualified Data.Map as Map
import Data.Monoid
import qualified Data.Set as Set
import qualified Data.Sequence as SQ
import System.Random

-- !!! HACKY CODE !!! WORKS WITH FOUR DRONE SCENARIOS

{-
Meant as a hacky redo of my "High First Spanning Tree Policy" where sometimes drones fail and share territory

DONE:
propagate a boolean value called maybeAnom through my two policy types, get everything compiling again
manage reassigning the results of kMeans to respect the type of anomaly present, if any
redo AdaptiveLowBFSPolicyAn so that it maintains the same territory confusion from HightFirst

TODO:
tweak the probability in rollNewAnomaly as needed
-}

--newly introduced Maybe parameter says whether we are in non-anomalous mode (Nothing) or have a particular anomaly (Just 1 2, etc.)
data HighFirstBFSPolicyAn = HighFirstBFSPolicyAn AltitudePhase StdGen (Map.Map DTPath Footprint) SubstAnomaly

instance Policy HighFirstBFSPolicyAn where
  nextMove pol@(HighFirstBFSPolicyAn LowSweep _ _ _) wv = --in this case, just do whatever the AdaptiveBFSPolicyAn does and repackage
    (nextMoves, repackagedPol)
    where
      repackagedPol = fromALBPAn nextLowPol
      (nextMoves, nextLowPol) = nextMove (toALBPAn pol) wv

  nextMove (HighFirstBFSPolicyAn HighSweep gen map maybeAnom) wv@(WorldView envInfo enStat) =  --the real novelty of this policy
    if (not anythingToDo)
      then nextMove (HighFirstBFSPolicyAn LowSweep gen kMeansResetMap maybeAnom) wv
      else (nas, HighFirstBFSPolicyAn HighSweep newPolicyGen newMap maybeNewAnom)
    where
      (nas, newMap) = Map.foldrWithKey (accumNextActionsAndMap High wv) ([], Map.empty) $ Map.mapKeys (removeObservedWaypoints toDo) anomalousMap

      anomalousMap = anomalizeMap maybeNewAnom actionableMap
        --map a function that will look for a DTPath with the "recpient drone", give it territory found by seeking out the current territory of the "target" drones

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

      anythingToDo = not $ Set.null toDo
      toDo = unseenLocations envInfo --what is still worth vising from the standpoint of a high policy
      blankSlate (DTPath dt _ _) = DTPath dt [] []
      blankMap = Map.fromSet (const Set.empty) $ Set.map blankSlate $ Map.keysSet map
      kMeansResetMap = fmap (detailedSetFromQuadCenters 2 (incompleteLocations envInfo)) $ kMeansInternal ((coarseQuadrantCenters 2) . incompleteLocations) kmGen envInfo 10 blankMap--ok to use kmGen in two places because only one gets run

      maybeNewAnom = rollNewAnomaly errorGen maybeAnom
      (kmGen, newPolicyGen) = split successorGen
      (errorGen, successorGen) = split gen

anomalizeMap :: SubstAnomaly -> (Map.Map DTPath Footprint) -> (Map.Map DTPath Footprint)
anomalizeMap Nothing map = map
anomalizeMap (Just (recipient, target)) map = newMap
  where
    --step 1: find the territory associated with "target"
    targetTerritory = snd $ head $ Map.toList $ Map.filterWithKey (\dtp@(DTPath dt _ _) -> \a -> target == (getDrone dt)) map
    --step 2: find the DTPath associated with "recipient" and change its territory to what "target" originally had
    newMap = Map.mapWithKey (\dtp@(DTPath dt _ _) -> \a -> if (recipient == (getDrone dt)) then targetTerritory else a) map --inefficient but OK

--the HighFirstBFSPolicy that is functionally identical to the given ALBP
fromALBPAn :: AdaptiveLowBFSPolicyAn -> HighFirstBFSPolicyAn
fromALBPAn (AdaptiveLowBFSPolicyAn gen map mA) = HighFirstBFSPolicyAn LowSweep gen map mA

--this will ignore the altitude phase, and then fromALBP will always return one in low mode
toALBPAn :: HighFirstBFSPolicyAn -> AdaptiveLowBFSPolicyAn
toALBPAn (HighFirstBFSPolicyAn _ gen map mA) = AdaptiveLowBFSPolicyAn gen map mA

initializeHFBFSPAn :: Int -> StdGen -> WorldView -> HighFirstBFSPolicyAn
initializeHFBFSPAn iterations gen wv@(WorldView envInfo enStat) = HighFirstBFSPolicyAn HighSweep polGen (fmap (detailedSetFromQuadCenters 3 (incompleteLocations envInfo)) $ kMeansAlt High iterations kmGen envInfo dtpSeq) Nothing
  where
    dtpSeq = SQ.fromList $ fmap (\(drone, pos) -> DTPath (DroneTerritory drone pos) [] []) $ fmap (fmap groundPos) enStat
    (kmGen, polGen) = split gen

data AdaptiveLowBFSPolicyAn = AdaptiveLowBFSPolicyAn StdGen (Map.Map DTPath Footprint) SubstAnomaly

type SubstAnomaly = Maybe (Drone, Drone)

rollNewAnomaly :: StdGen -> SubstAnomaly -> SubstAnomaly
rollNewAnomaly erg oldAn = case oldAn of
  Nothing -> if (errorVal > threshold)
    then Just (DroneID recipient, DroneID target)
    else oldAn
    where
      (errorVal, nextGen) = randomR (0 :: Float, 1) erg
      threshold = (9 :: Float) / 2000
      (recipient, threeGen) = randomR (1 :: Int, 4) nextGen
      target = head $ dropWhile (== recipient) $ randomRs (1 :: Int, 4) threeGen
  Just _ -> oldAn

instance Policy AdaptiveLowBFSPolicyAn where
  nextMove p@(AdaptiveLowBFSPolicyAn gen map maybeAnom) wv@(WorldView envInfo enStat) = (nas, AdaptiveLowBFSPolicyAn newPolicyGen newMap maybeNewAnom)
    where

      --step 6: command the previously computed directions to any idle drone
      --step 5: use A* (with penalties?) to give directions to the next waypoint for any IDLE drone that does not already have directions (after step 4)
      (nas, newMap) = Map.foldrWithKey (accumNextActionsAndMap Low wv) ([], Map.empty) anomalousMap

      anomalousMap = anomalizeMap maybeNewAnom actionableMap
      
      actionableMap =
        if anyDroneNeedsTerritory
          then 
            --step 4: if kMeans got run, readjust the keys of this map to contain newly developed paths in light of the new territory shapes and envInfo
            Map.foldrWithKey (refreshWaypoints lowSmartEdgeBFSCoarsePath enStat boundsSet) Map.empty $
              --step 3: run a few iterations of kMeansInternal (IF it was deemed necessary below) to determine the new values on this map
              fmap (detailedSetFromQuadCenters 1 incompleteLocs) $ kmiByFootprint kmGen (coarseQuadrantCenters 1 incompleteLocs) 4 filteredWaypointsMap --4 is an arbitrary choice for number of kMeans iterations
          else filteredWaypointsMap

      --step 2: determine if it is a good time to do territory re-assignment
      anyDroneNeedsTerritory = getAny $ foldMap (Any . (dtpNeedsNewMoves enStat)) $ Map.keysSet filteredWaypointsMap
        --only run kMeans if a drone has run out of territory to explore, necessitating a re-plan
        --once the not-wirth-visiting waypoints have been pruned for this time step, this is a simple matter of checking if both the waypoints and directions are empty and the drone is idle

      --step 1: in light of the most recent envInfo, filter the waypoints list of each drone to just those positions that still merit a visit
      filteredWaypointsMap = Map.mapKeys removeF currentDronesMap
      
      removeF = filterWaypointsByFootprint incompleteLocs envInfo

      --step 0: check the existing map entries with the current ensembleStatus in case a drone has dropped out
      currentDronesMap = Map.filterWithKey (droneInSet aliveDrones) map
      aliveDrones = Set.fromList $ fmap fst enStat

      incompleteLocs = incompleteLocations envInfo
      boundsSet = toFootprint envInfo

      maybeNewAnom = rollNewAnomaly errorGen maybeAnom
      (kmGen, newPolicyGen) = split successorGen
      (errorGen, successorGen) = split gen

testComp = "."