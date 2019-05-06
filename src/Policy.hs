module Policy where

import WorldState
import Ensemble
import Drone

import Data.Maybe

class Policy p where
  nextMove :: p -> WorldState -> NextActions

--how to expose a policy in the process of being learned? IO Policy? State?

newtype PolicyMap = PolicyMap [(WorldState, NextActions)]

instance Policy PolicyMap where
  nextMove (PolicyMap policyMap) worldState =  fromMaybe defaultActions $ lookup worldState policyMap
    where defaultActions = [(drone, Hover) | drone <- (getDroneList worldState)]