module ShapeSweepAgent where

--agents that execute paths based purely on the footprint of the environment

data LowSweepPolicy = LowSweepPolicy

--policy should plan a path to the lowest ord - valued patch that has not been fully observed

--a second, "HighSweepPolicy" should probably use the same policy as LowSweep for its "phase 2" behavior, after the high environment has been explored and it has descended again


testCompilation = "!"