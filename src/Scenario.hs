module Scenario where

import qualified Data.Map.Strict as Map --unionWith
import qualified Data.Set as Set
import Data.Monoid

import Env
import EnvView
import WorldState
import Drone

--takes an environment, a number of drones to spawn, and creates a WorldState representing a completely unexplored scenario
initializeWorldState :: Integer -> Environment -> WorldState
initializeWorldState i env = WorldState env info enStat
  where
    info = EnvView.initializeInfo env
    minPosition = Set.findMin . Map.keysSet . getMap $ env--this might fail for empty environments? --type of this is Position
    enStat = fmap numberDrone [1 .. i]
    numberDrone n = (DroneID n, Assigned Hover (DronePos minPosition Low))

isTerminal :: WorldState -> Bool
isTerminal ws = isCompleteInfo (getInfo ws)

isCompleteInfo :: EnvironmentInfo -> Bool
isCompleteInfo envInfo = getAll $ foldMap (All . EnvView.isFullyObserved) $ Map.elems envInfo