module ShapeSweepAgent where

--agents that execute paths based purely on the footprint of the environment

data LowSweepPolicy = LowSweepPolicy

--policy should plan a path to the lowest ord - valued patch that has not been fully observed

testCompilation = "!"