module LogScenario where

{-
Basically, this makes a variant of the scenario management code (i.e. runScenario) that logs information about the WorldState at each time step
Then, it transforms the minimal information associated with each time step into a "row" that includes some derived quantities
-}

import Scenario



testMe = "."