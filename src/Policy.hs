module Policy where

import WorldState
import Ensemble

class Policy p where
  nextMove :: p -> WorldState -> EnsembleStatus

