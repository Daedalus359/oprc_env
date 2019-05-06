module Policy where

import WorldState
import EnvView
import Ensemble
import Drone

import Data.Maybe

class Policy p where
  nextMove :: p -> WorldView -> NextActions

--how to expose a policy in the process of being learned? IO Policy? State?

newtype PolicyMap = PolicyMap [(WorldView, NextActions)]

instance Policy PolicyMap where
  nextMove (PolicyMap policyMap) worldView =  fromMaybe defaultActions $ lookup worldView policyMap
    where defaultActions = [(drone, Hover) | drone <- (EnvView.getDroneList worldView)]