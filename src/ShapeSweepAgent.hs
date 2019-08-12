module ShapeSweepAgent where

import GraphOPRC

--agents that execute paths based purely on the footprint of the environment

data LowSweepPolicy = LowSweepPolicy
 {
   getRemainingPath :: Path
 }

--policy should plan a path to the lowest ord - valued patch that has not been fully observed

--a second, "HighSweepPolicy" should probably use the same policy as LowSweep for its "phase 2" behavior, after the high environment has been explored and it has descended again
--highsweepPolicy should use a smart function to query A* for paths to a filtered subset of nodes that make sense to visit (e.g rows 2, 5, 8, etc. with shape caveats)
