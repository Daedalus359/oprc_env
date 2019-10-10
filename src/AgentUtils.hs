module AgentUtils where

import Ensemble
import Env
import EnvView
import Drone
import GraphOPRC
import Policy
import WorldState

import Data.Maybe
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import System.Random

testCompilation = "do it!"

--outputs Directions if all of the hops between path elements correspond to atomic movements in the sim
makeDirections :: Path -> Maybe Directions
makeDirections [] = Just []
makeDirections (pos : []) = Just []
makeDirections (pos1 : rest@(pos2 : path)) = 
  case (toAction (getHop pos1 pos2)) of
    Nothing -> Nothing
    (Just action) -> fmap ((:) action) $ makeDirections rest

class Policy p => DroneTerritoryMapPolicy p where
  getMap :: p -> Map.Map DroneTerritory Footprint
  fromMap :: StdGen -> Map.Map DroneTerritory Footprint -> p

applyMoves :: DroneTerritoryMapPolicy p => EnsembleStatus -> StdGen -> Map.Map DroneTerritory Footprint -> (NextActions, p)
applyMoves enStat gen map = (nextActions, policy)
  where
    policy = fromMap gen $ Map.fromAscList newMapList
    (newMapList, nextActions) = foldr (accumulateNextMoves enStat) ([], []) mapList--foldr should preserve ascending nature of mapList

    mapList :: [(DroneTerritory, Footprint)]
    mapList = Map.toAscList map

accumulateNextMoves :: EnsembleStatus -> (DroneTerritory, Footprint) -> ([(DroneTerritory, Footprint)], NextActions) -> ([(DroneTerritory, Footprint)], NextActions)
accumulateNextMoves enStat (dt@(DroneTerritory drone mean dirs), fp) (assignmentListSoFar, nextActionsSoFar) =
  if droneIsIdle --only add to the list of NextActions if a drone is idle. Actions given to non idle drones may be discarded, and non idle drones are not guaranteed to have next actions
    then ((newDt, fp) : assignmentListSoFar, newActionAssignment : nextActionsSoFar)
    else ((dt, fp) : assignmentListSoFar, nextActionsSoFar)
  where
    --these should only be evauluated when the current drone is idle, which should mean dirs matches the (head : tail) pattern
    newActionAssignment = (drone, newAction)
    newAction = head dirs
    newDt = DroneTerritory drone mean (tail dirs)

    droneStat = fromJust $ lookup drone enStat --the lookup operation should never fail to find the drone's real status
    droneIsIdle = isUnassigned droneStat --will this drone need a new action assignment during this nextMove step?

type DirectionsFunc = WorldView -> Set.Set DroneTerritory -> DroneTerritory -> Footprint -> DroneTerritory

--uses A* and the current territory assignments to assign what the idle and unassigned drones should do next
assignDirections :: DirectionsFunc -> WorldView -> Map.Map DroneTerritory Footprint -> Map.Map DroneTerritory Footprint
assignDirections dirF wv map = Map.fromAscList listWithDirections
  where
    listWithDirections :: [(DroneTerritory, Footprint)]
    listWithDirections = fmap (\(dt, fp) -> (setF dt fp, fp)) mapList --preserves the Ascending property of the keys in this list

    setF = dirF wv meansSet

    mapList :: [(DroneTerritory, Footprint)]
    mapList = Map.toAscList map

    meansSet = Map.keysSet map

--many policies assume that drones always fly low. This provides an extra layer of protection to that assumption at every action assignment step
fixAltLow :: Altitude -> [Action] -> [Action] --this should make sure that the drones always fly low
fixAltLow Low al = al
fixAltLow High al = (MoveVertical Descend) : al

--for agents that move between a high sweeping phase and a low sweeping phase
data AltitudePhase = HighSweep | LowSweep

data DronePhase = DronePhase AltitudePhase DroneTerritory