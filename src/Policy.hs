module Policy where

import WorldState
import Ensemble

nextMove :: WorldState -> EnsembleStatus
--CHANGE ME! currently does not update status at all
nextMove ws = (ensembleStatus ws) 
