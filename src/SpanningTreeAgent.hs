module SpanningTreeAgent where

import Drone
import Ensemble
import Env
import EnvView
import GraphOPRC
import Policy

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

      supplyDirections :: EnvironmentInfo -> EnsembleStatus -> Drone -> Directions -> Directions
      supplyDirections _ _ _ dirs@(dir : more) = dirs --don't need to add directions if the drone already has some to follow
      supplyDirections envInfo enStat drone [] = undefined
        --figure out the footprint of places worth visiting
        --pass that footprint to the spanning forest path creation function
        --fill in all non-atomic gaps in that path with A*, including the path from current drone position to root
        --convert that path to directions

      --are there any drones for which no additional actions have been pre-computed?
      anyDronesLackingDirs = foldMap (\dirs -> Any $ null dirs) map