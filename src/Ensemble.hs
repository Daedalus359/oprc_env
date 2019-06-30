module Ensemble where

import Drone
import Data.Map as Map

type DroneList = [Drone]

type EnsembleStatus = [(Drone, DroneStatus)]