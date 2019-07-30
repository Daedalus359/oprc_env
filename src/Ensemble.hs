module Ensemble where

import Drone
import Data.Map.Strict as Map

type DroneList = [Drone]

type EnsembleStatus = [(Drone, DroneStatus)]

--list the drones which are unassigned in the given EnsembleStatus
needsCommand :: EnsembleStatus -> [Drone]
needsCommand [] = []
needsCommand ((drone, Unassigned _) : ensStat) = drone : (needsCommand ensStat)
needsCommand ((drone, _) : ensStat) = needsCommand ensStat