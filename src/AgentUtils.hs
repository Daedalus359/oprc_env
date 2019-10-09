module AgentUtils where

import Env
import Drone
import GraphOPRC
import Policy

import qualified Data.Map.Strict as Map
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

--for agents that move between a high sweeping phase and a low sweeping phase
data SweepPhase = HighSweep | LowSweep

data DronePhase = DronePhase SweepPhase DroneTerritory