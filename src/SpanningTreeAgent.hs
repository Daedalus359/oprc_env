module SpanningTreeAgent where

import AgentUtils
import Drone
import Ensemble
import Env
import EnvView
import GraphOPRC
import Policy
import WorldState

import Data.Maybe
import Data.Monoid
import qualified Data.Set as Set
import qualified Data.Map as Map
import System.Random


--this policy explores a spanning tree that minimizes covering old ground
data LowSpanningTreePolicy = LowSpanningTreePolicy (Map.Map Drone Directions)

instance Policy LowSpanningTreePolicy where
  nextMove p@(LowSpanningTreePolicy map) wv@(WorldView envInfo enStat) =
    if (null unassignedDrones)
      then ([], p)--all of the drones are acting, so no computation of next moves is necessary
      else assignMoves enStat directionsMap --make sure all drones have directions queued, then assign from their directions

    where
      unassignedDrones = needsCommand enStat

      --go through the map and make sure that all drones have a non empty list of actions to perform
      directionsMap = Map.mapWithKey (supplyDirections envInfo enStat) map

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
supplyDirections :: EnvironmentInfo -> EnsembleStatus -> Drone -> Directions -> Directions
supplyDirections _ _ _ dirs@(dir : more) = dirs --don't need to add directions if the drone already has some to follow
supplyDirections envInfo enStat drone [] = 
  if (null needsVisit)
    then [Hover]
    else case newDirections of
      Nothing -> [Hover]
      Just dirs -> dirs
  where
  --figure out the footprint of places worth visiting
  needsVisit = incompleteLocations envInfo
  minLoc = Set.findMin needsVisit

  --figure out which of those is closest to the drone's current position
  currentStatus = lookup drone enStat
  currentGroundPos = fmap groundPos currentStatus
  closestPos = case currentGroundPos of
    Nothing -> minLoc
    Just pos -> foldr (closerTo pos) minLoc needsVisit

  --pass that footprint to the spanning forest path creation function
  sfPath = customRootInBoundsSpanningTreePath 2 needsVisit closestPos

  --fill in all non-atomic gaps in that path with A*, including the path from current drone position to root
  atomicPath = toAtomicPath (Map.keysSet envInfo) closestPos sfPath --use the full footprint so that it has access to the full bounds

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