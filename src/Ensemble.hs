module Ensemble where

import Drone
import Data.Map as Map

data DroneID = Drone Integer
  deriving Eq

type DroneList = [DroneID]

type EnsembleStatus = [(DroneID, DroneStatus)]