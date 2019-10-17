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

class Policy p => CachedDirectionsPolicy p where
  getDirs :: p -> Directions
  setDirs :: p -> Directions -> p

class Policy p => DroneTerritoryMapPolicy p where
  getMap :: p -> Map.Map DTDirs Footprint
  fromMap :: StdGen -> Map.Map DTDirs Footprint -> p

applyMoves :: DroneTerritoryMapPolicy p => EnsembleStatus -> StdGen -> Map.Map DTDirs Footprint -> (NextActions, p)
applyMoves enStat gen map = (nextActions, policy)
  where
    policy = fromMap gen $ Map.fromAscList newMapList
    (newMapList, nextActions) = foldr (accumulateNextMoves enStat) ([], []) mapList--foldr should preserve ascending nature of mapList

    mapList :: [(DTDirs, Footprint)]
    mapList = Map.toAscList map

accumulateNextMoves :: EnsembleStatus -> (DTDirs, Footprint) -> ([(DTDirs, Footprint)], NextActions) -> ([(DTDirs, Footprint)], NextActions)
accumulateNextMoves enStat (dtd@(DTDirs dt@(DroneTerritory drone mean) dirs), fp) (assignmentListSoFar, nextActionsSoFar) =
  if droneIsIdle --only add to the list of NextActions if a drone is idle. Actions given to non idle drones may be discarded, and non idle drones are not guaranteed to have next actions
    then ((newDTD, fp) : assignmentListSoFar, newActionAssignment : nextActionsSoFar)
    else ((dtd, fp) : assignmentListSoFar, nextActionsSoFar)
  where
    --these should only be evauluated when the current drone is idle, which should mean dirs matches the (head : tail) pattern
    newActionAssignment = (drone, newAction)
    newAction = head dirs
    newDTD = DTDirs dt (tail dirs)

    droneStat = fromJust $ lookup drone enStat --the lookup operation should never fail to find the drone's real status
    droneIsIdle = isUnassigned droneStat --will this drone need a new action assignment during this nextMove step?

type DirectionsFunc = WorldView -> Set.Set DTDirs -> DTDirs -> Footprint -> DTDirs

--uses A* and the current territory assignments to assign what the idle and unassigned drones should do next
assignDirections :: DirectionsFunc -> WorldView -> Map.Map DTDirs Footprint -> Map.Map DTDirs Footprint
assignDirections dirF wv map = Map.fromAscList listWithDirections
  where
    listWithDirections :: [(DTDirs, Footprint)]
    listWithDirections = fmap (\(dtd, fp) -> (setF dtd fp, fp)) mapList --preserves the Ascending property of the keys in this list

    setF = dirF wv meansSet

    mapList :: [(DTDirs, Footprint)]
    mapList = Map.toAscList map

    meansSet = Map.keysSet map

--many policies assume that drones always fly low. This provides an extra layer of protection to that assumption at every action assignment step
fixAltLow :: Altitude -> [Action] -> [Action] --this should make sure that the drones always fly low
fixAltLow Low al = al
fixAltLow High al = (MoveVertical Descend) : al

--for agents that move between a high sweeping phase and a low sweeping phase
data AltitudePhase = HighSweep | LowSweep

data DronePhase = DronePhase AltitudePhase DroneTerritory

--finds a position's distance to the closest of the means position from the current drone policy that *don't* correspond to the one being altered
leastDistMeans :: HasCenter c =>  Set.Set c -> Position -> Int
leastDistMeans otherMeans p = 
  if (Set.null otherMeans)
    then 0
    else minimum $ distanceFs <*> (pure p)
  where
    distanceFs = fmap idealDistance $ Set.toList $ Set.map getCenter otherMeans