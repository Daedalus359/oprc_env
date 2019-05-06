module Ensemble where

import Drone
import Data.Map as Map

data Drone = DroneID Integer
  deriving (Eq, Show)

type DroneList = [Drone]

type EnsembleStatus = [(Drone, DroneStatus)]