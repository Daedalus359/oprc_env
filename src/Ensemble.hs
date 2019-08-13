module Ensemble where

import Drone
import qualified Data.Map.Strict as Map

type DroneList = [Drone]

type EnsembleStatus = [(Drone, DroneStatus)]

--list the drones which are unassigned in the given EnsembleStatus
needsCommand :: EnsembleStatus -> [Drone]
needsCommand [] = []
needsCommand ((drone, Unassigned _) : ensStat) = drone : (needsCommand ensStat)
needsCommand ((drone, _) : ensStat) = needsCommand ensStat

--the opposide of needsCommand
occupiedDrones :: EnsembleStatus -> [Drone]
occupiedDrones [] = []
occupiedDrones ((drone, Unassigned _) : ensStat) = occupiedDrones ensStat
occupiedDrones ((drone, _) : ensStat) = drone : (occupiedDrones ensStat)

allIdle :: EnsembleStatus -> Bool
allIdle = foldr (\(drone, status) -> (&& isUnassigned status)) True 