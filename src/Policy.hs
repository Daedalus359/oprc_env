module Policy where

import WorldState
import Ensemble
import Drone

import Data.Maybe

class Policy p where
  nextMove :: p -> WorldState -> EnsembleStatus

newtype PolicyMap = PolicyMap [(WorldState, EnsembleStatus)]

instance Policy PolicyMap where
  nextMove (PolicyMap policyMap) worldState =  fromMaybe defaultStatus $ lookup worldState policyMap
    where defaultStatus = [(drone, Unassigned) | drone <- (getDroneList worldState)]