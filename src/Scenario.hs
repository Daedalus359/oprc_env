module Scenario where

import qualified Data.Map.Strict as Map --unionWith
import qualified Data.Set as Set
import Data.Monoid

import Env
import EnvView
import WorldState
import Drone
import Policy
import Ensemble

--p should be an instance of the policy typeclass, see mkScenatio
data Scenario p = 
  Scenario {
    getPolicy :: p
  , getWorldState :: WorldState
  , getTime :: Integer
  , getHist :: MoveHistory
  }
  deriving (Eq, Show)

mkScenario :: (Policy p) => p -> Integer -> Environment -> Scenario p
mkScenario policy numDrones env = Scenario policy (initializeWorldState numDrones env) 0 []

data Snapshot = 
  Snapshot {
    getCommands :: NextActions
  , getTimeStamp :: Integer
  }
  deriving (Eq, Show)

stepScenario :: (Policy p) => Scenario p -> Scenario p
stepScenario (Scenario policy ws time hist) = Scenario nextPolicy newWs (time + 1) newHist
  where
    currentWV = toView ws
    (nextMoves, nextPolicy) = nextMove policy currentWV
    newWs = updateState nextMoves ws
    newHist = case nextMoves of
                    [] -> hist
                    na@(move : moves) -> (Snapshot na time) : hist
  --if the nextActions is non-empty, add it and the current timestamp to the History (add the possibly updated history to the result)

--ideally, a History will contain minimal information required to recreate the entire sequence of events, given the Environment and other info in the Scenario
type MoveHistory = [Snapshot]

-- below are some utlilty functions to help start and end a scenario --------------------------------------------------------------------

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