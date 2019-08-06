module ShapeSweepAgent where

--agents that execute paths based purely on the footprint of the environment

data LowSweepPolicy = LowSweepPolicy

--policy should plan a path to the lowest ord - valued patch that has not been fully observed
--need memoization for efficiency, not an immediate priority
--should probably use Manhattan distance as a heuristic
--use inBoundsNeighborsOf
--open set represents the search frontier


testCompilation = "!"