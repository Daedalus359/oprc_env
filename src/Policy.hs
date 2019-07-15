module Policy where

import WorldState
import EnvView
import Ensemble
import Drone

import Data.Maybe

class Policy p where
  nextMove :: p -> WorldView -> (NextActions, p)
  --p can encode how it will learn from the next WorldView it gets to see as a function of the most recent NextActions it sent
    --the above is not necessary for an implementation of Policy, but is probably required to learn an effective policy in complex environments


--how to expose a policy in the process of being learned? IO Policy? State?

newtype PolicyMap = PolicyMap [(WorldView, NextActions)]

instance Policy PolicyMap where
  nextMove p@(PolicyMap policyMap) worldView = (nextActions, p)
    where
      nextActions = fromMaybe defaultActions $ lookup worldView policyMap
      defaultActions = [(drone, Hover) | (drone, _) <- (EnvView.getEnsembleStatus worldView)]