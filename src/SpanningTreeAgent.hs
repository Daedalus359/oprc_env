module SpanningTreeAgent where

import Drone
import Ensemble
import Env
import EnvView
import GraphOPRC
import Policy

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
      else assignMoves directionsMap --make sure all drones have directions queued, then assign from their directions

    where
      unassignedDrones = needsCommand enStat

      assignMoves = undefined

      --go through the map and make sure that all drones have a non empty list of actions to perform
      directionsMap = Map.mapWithKey (supplyDirections envInfo enStat) map

      --are there any drones for which no additional actions have been pre-computed?
      anyDronesLackingDirs = getAny $ foldMap (\dirs -> Any $ null dirs) map

--use spanning forest based path generation to give directions to a drone if it lacks them
supplyDirections :: EnvironmentInfo -> EnsembleStatus -> Drone -> Directions -> Directions
supplyDirections _ _ _ dirs@(dir : more) = dirs --don't need to add directions if the drone already has some to follow
supplyDirections envInfo enStat drone [] = 
  if (null needsVisit)
    then [Hover]
    else undefined
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
  atomicPath = toAtomicPath (Map.keysSet envInfo) --use the full footprint so that it has access to the full bounds

  --convert that path to directions

--takes the waypoints generated by spanning forest traversals and fills in the details, connecting it to a start position
toAtomicPath :: Footprint -> Position -> Path -> Path
toAtomicPath fp startPos [] = undefined