module SpanningTreeAgent where

import AgentUtils
import Drone
import Ensemble
import Env
import EnvView
import GraphOPRC
import Policy
import SpanningLoop
import WorldState

import Data.List
import Data.Maybe
import Data.Monoid
import qualified Data.Sequence as SQ
import qualified Data.Set as Set
import qualified Data.Map as Map
import System.Random


--this policy explores a spanning tree that minimizes covering old ground
data LowSpanningTreePolicy = LowSpanningTreePolicy (Map.Map Drone Directions)
  deriving Show

initializeLSTP :: WorldView -> LowSpanningTreePolicy
initializeLSTP wv@(WorldView envInfo enStat) = LowSpanningTreePolicy $ Map.fromSet (const [MoveVertical Descend, Hover]) dronesList
  where
    dronesList :: Set.Set Drone
    dronesList = Set.fromList $ fmap fst enStat

instance Policy LowSpanningTreePolicy where
  nextMove p@(LowSpanningTreePolicy map) wv@(WorldView envInfo enStat) =
    if (null unassignedDrones)
      then ([], p)--all of the drones are acting, so no computation of next moves is necessary
      else assignMoves enStat directionsMap --make sure all drones have directions queued, then assign from their directions

    where
      unassignedDrones = needsCommand enStat

      --go through the map and make sure that all drones have a non empty list of actions to perform
      directionsMap = Map.mapWithKey lowerIfNeeded $ Map.mapWithKey (supplyDirections Low envInfo enStat) map


      lowerIfNeeded drone directions =
        if isHigh
          then (MoveVertical Descend) : directions
          else directions
        where
          isHigh = case (lookup drone alts) of
            Just High -> True
            _ -> False

          alts = fmap (fmap $ getEnvAlt . posFromStat) $ enStat

      --are there any drones for which no additional actions have been pre-computed?
      anyDronesLackingDirs = getAny $ foldMap (\dirs -> Any $ null dirs) map

assignMoves :: EnsembleStatus -> Map.Map Drone Directions -> (NextActions, LowSpanningTreePolicy)
assignMoves enStat map = (nextActions, LowSpanningTreePolicy newMap)
  where
    (nextActions, newMap) = Map.foldrWithKey accumulateMovesAndMap ([], Map.empty) map
    accumulateMovesAndMap drone directions (naSoFar, newMapSoFar) = (newNA, newMap)
      where
        newNA = (drone, head directions) : naSoFar
        newMap = Map.insert drone (tail directions) newMapSoFar


--use spanning forest based path generation to give directions to a drone if it lacks them
supplyDirections :: Altitude -> EnvironmentInfo -> EnsembleStatus -> Drone -> Directions -> Directions
supplyDirections _ _ _ _ dirs@(dir : more) = dirs --don't need to add directions if the drone already has some to follow
supplyDirections altitudeMODE envInfo enStat drone [] = 
  if (null needsVisit)
    then [Hover]
    else case newDirections of
      Nothing -> [Hover]
      Just dirs@(dir:more) -> dirs
      Just [] -> [Hover]
  where
  --figure out the footprint of places worth visiting
  needsVisit = 
    case altitudeMODE of
      Low -> incompleteLocations envInfo
      High -> unseenLocations envInfo
  minLoc = Set.findMin needsVisit

  --figure out which of those is closest to the drone's current position
  currentStatus = lookup drone enStat
  currentGroundPos = fmap groundPos currentStatus
  closestPos = case currentGroundPos of
    Nothing -> minLoc
    Just pos -> foldr (closerTo pos) minLoc needsVisit

  --pass that footprint to the spanning forest path creation function
  coarseness =
    case altitudeMODE of
      Low -> 2
      High -> 6
  sfPath = customRootInBoundsSpanningTreePath coarseness needsVisit closestPos

  --fill in all non-atomic gaps in that path with A*, including the path from current drone position to root
  atomicPath = toAtomicPath (Map.keysSet envInfo) (fromMaybe closestPos currentGroundPos) sfPath --use the full footprint so that it has access to the full bounds

  --convert that path to directions
  newDirections = atomicPath >>= makeDirections

toAtomicPath :: Footprint -> Position -> Path -> Maybe Path
toAtomicPath fp startPos waypoints = fmap ((:) startPos) tailPath
  where
    tailPath = toAtomicPathInternal fp startPos waypoints

--takes the waypoints generated by spanning forest traversals and fills in the details, connecting it to a start position
toAtomicPathInternal :: Footprint -> Position -> Path -> Maybe Path
toAtomicPathInternal fp startPos [] = Just []
toAtomicPathInternal fp startPos (waypoint : more) =
  case (firstStep) of
    Nothing -> Nothing
    Just path -> fmap ((++) path) $ toAtomicPathInternal fp waypoint more
  where
    --fmap tail means that startPos does not get included in the path. 
    --Having this behavior in the recursive step makes it easier to combine outputs
    --startPos needs to be included if it is the actual top level start position
    firstStep = fmap tail $ aStarByFootprint fp mkManhattanHeuristic startPos waypoint

data LowKMeansSpanningTreePolicy = LowKMeansSpanningTreePolicy StdGen (Map.Map DTDirs Footprint)
  deriving Show

initializeLKMSTP :: Int -> StdGen -> WorldView  -> LowKMeansSpanningTreePolicy
initializeLKMSTP iterations gen wv@(WorldView envInfo enStat) = LowKMeansSpanningTreePolicy gen2 $ kMeansLow iterations gen1 envInfo dSeq
  where
    dSeq = SQ.fromList $ fmap (\(drone, pos) -> DTDirs (DroneTerritory drone pos) []) dronesList

    dronesList :: [(Drone, Position)]
    dronesList = fmap (fmap groundPos) enStat

    (gen1, gen2) = split gen

instance Policy LowKMeansSpanningTreePolicy where
  nextMove p@(LowKMeansSpanningTreePolicy gen map) wv@(WorldView envInfo enStat) = applyMoves enStat gen2 directedMap

    where
      --directedMap must ensure that all drones are either acting or have a list of next actions to draw from
      directedMap = assignDirections setDirectionsBySpanningPath wv reassignedMap

      --opportunity to run kMeansInternal here, as in KMeansLowPolicy. Should I be doing this every time?
      reassignedMap = kMeansInternal incompleteLocations gen1 envInfo 1 map 
      (gen1, gen2) = split gen

instance DroneTerritoryMapPolicy LowKMeansSpanningTreePolicy where
  getMap (LowKMeansSpanningTreePolicy gen map) = map
  fromMap = LowKMeansSpanningTreePolicy

data LowWaypointsKMeansSpanningTreePolicy = LowWaypointsKMeansSpanningTreePolicy StdGen 

--the second parameter, meansSet, was useful for ShapeSweepAgent to prioritize exploring territory that was far from all other territory means
  --it is probably useful here as well, but I won't use it for now
setDirectionsBySpanningPath :: (HasDroneTerritory d, HasCachedDirs d) => WorldView -> Set.Set d -> d -> Footprint -> d
setDirectionsBySpanningPath wv@(WorldView envInfo enStat) meansSet hasDT fp =
  if (droneIsIdle && outOfDirections)
    then setCachedDirs hasDT $ fixAltLow droneAlt newDirs
    else hasDT

  where
    dt@(DroneTerritory drone mean) = getDT hasDT
    dirs = getCachedDirs hasDT

    --these 3 definitions determine whether the drone being considered needs a new set of directions at all
    droneStat = fromJust $ lookup drone enStat --if the drone is not in the ensemble status, the problem is somewhere else
    droneIsIdle = isUnassigned droneStat
    outOfDirections =
      case dirs of
        (action : actions) -> False
        [] -> True

    droneAlt = getEnvAlt dronePos
    dronePos = posFromStat droneStat
    currentGroundPos = groundPos droneStat

    --the real difference from the k means only version. 
    newDirs = if (Set.null toVisit)
      then [Hover] --nothing intelligent to do if no unexplored territory has been assigned to this drone
      else case newDirections of
        Nothing -> [Hover]
        Just [] -> --[Hover]
          case (null sfPath) of
            False -> [MoveIntercardinal NW] --this rather than hover for easy diagnostics
            True -> [MoveIntercardinal NE] --this rather than hover for easy diagnostics
            --Nothing -> [MoveCardinal South]
        Just dirs@(dir:more) -> dirs

    --the set of locations that are in this drone's territory AND in the set of non-fully-explored locations in the scenario overall
    toVisit = Set.intersection fp $ incompleteLocations envInfo
    tvSize = Set.size toVisit

    --these need new versions!
    minLoc = Set.findMin toVisit --this shouldn't get run if toVisit is empty
    closestPos = foldr (closerTo currentGroundPos) minLoc toVisit

    --coarse version tries to keep the spanning trees as "filled in" with member positions as possible
    filledCurrentTreeSet = detailedSet 2 fp coarseCurrentTreeSet
    coarseCurrentTreeSet = Set.fromList $ drop (coarseTVSize - numToTakeC) sortedCTVL
    numToTakeC = max (quot coarseTVSize 2) $ min 6 coarseTVSize
    sortedCTVL = sortOn (leastDistMeans otherMeans) ctvList
    ctvList = Set.toList coarseToVist
    coarseToVist = coarseMap 2 toVisit
    coarseTVSize = Set.size coarseToVist

    minLoc3 = Set.findMin coarseCurrentTreeSet
    closestPos3 = foldr (closerTo currentGroundPos) minLoc3 coarseCurrentTreeSet

    --the subset of elements from toVisit which are farther from other means than the rest
    currentTreeSet = Set.fromList $ drop (tvSize - numToTake) sortedTVL --we want the positions which have the GREATEST distance to their closest foreign mean
    numToTake = max (quot tvSize 6) $ min 24 tvSize
    sortedTVL = sortOn (leastDistMeans otherMeans) tvList
    tvList = Set.toList toVisit
    otherMeans = Set.delete dt $ Set.map getDT meansSet

    minLoc2 = Set.findMin currentTreeSet
    closestPos2 = foldr (closerTo currentGroundPos) minLoc2 currentTreeSet

    boundsFP = toFootprint envInfo

    --use the above to run cardinal spanning forests and get a coarse path
    --the big difference from the single agent version is that this uses only patches in the drone's territory as the positions to span
    --sfPath = customRootInBoundsSpanningTreePath 2 filledCurrentTreeSet closestPos3

    --sfPath = lowBFSCoarsePath boundsFP filledCurrentTreeSet closestPos3
    sfPath = lowSmartEdgeBFSCoarsePath boundsFP toVisit closestPos
    --sfPath = lowSmartEdgeBFSCoarsePath boundsFP filledCurrentTreeSet closestPos3

    --refine the coarse path into a fully detailed one, then make directions from them
    atomicPath = toAtomicPath (Map.keysSet envInfo) currentGroundPos sfPath --relies of A*, so this is a maybe value
    newDirections = atomicPath >>= makeDirections --another maybe value

data AdaptiveLowBFSPolicy = AdaptiveLowBFSPolicy (Map.Map DTPath Footprint)

--(DTPath dt@(DroneTerritory drone mean) path directions)

--this policy uses BFS to plan approaches, but it saves the resulting plan as a path, rather than directions
--this allows the use of A* to check the next path entry before committing to going there on a certain path or at all
instance Policy AdaptiveLowBFSPolicy where
  nextMove p@(AdaptiveLowBFSPolicy map) wv@(WorldView envInfo enStat) = undefined
    where
      --step 6: command the previously computed directions to any idle drone

      --step 5: use A* (with penalties?) to give directions to the next waypoint for any IDLE drone that does not already have directions (after step 4)

      --step 4: if kMeans got run, readjust the keys of this map to contain newly developed paths in light of the new territory shapes and envInfo

      --step 3: run a few iterations of kMeansInternal (IF it was deemed necessary below) to determine the new values on this map

      --step 2: determine if it is a good time to do territory re-assignment
      anyDroneNeedsTerritory = undefined
        --only run kMeans if a drone has run out of territory to explore, necessitating a re-plan
        --once the not-wirth-visiting waypoints have been pruned for this time step, this is a simple matter of checking if both the waypoints and directions are empty and the drone is idle

      --step 1: in light of the most recent envInfo, filter the waypoints list of each drone to just those positions that still merit a visit


--probably makes sense to create a function that explores all high then all low for now
data HighFirstSpanningTreePolicy = HighFirstSpanningTreePolicy (Map.Map DronePhase Directions)
--data HighFirstSpanningTreePolicy = HighFirstSpanningTreePolicy (Map.Map DroneTerritory Directions) (Map.Map Drone SweepPhase)

-- write code that "cleans up" after KMeans by reassigning individual patches to the drone that will be traversin its center. 
-- But can I know which drone that will be if I make the alignment of the spanning trees custoizable and adaptive?