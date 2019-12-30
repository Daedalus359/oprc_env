module Scenario where

import qualified Data.Map.Strict as Map --unionWith
import qualified Data.Set as Set
import Data.Monoid
import System.Random

import Env
import EnvView
import FisherYatesShuffle
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

data RandomScenario p = RandomScenario {rsGen :: StdGen, rsScenario :: (Scenario p)}

mkScenario :: (Policy p) => (WorldView -> p) -> Int -> Environment -> Scenario p
mkScenario policyF numDrones env = Scenario (policyF wv) ws 0 []
  where
    wv = toView ws
    ws = initializeWorldState numDrones env

mkRandomScenario :: (Policy p) => (WorldView -> p) -> Int -> Environment -> StdGen -> RandomScenario p
mkRandomScenario pf nd env gen = RandomScenario gen $ mkScenario pf nd env

data Snapshot = 
  Snapshot {
    getCommands :: NextActions
  , getTimeStamp :: Integer
  }
  deriving (Eq, Show)

--ideally, a History will contain minimal information required to recreate the entire sequence of events, given the Environment and other info in the Scenario
type MoveHistory = [Snapshot]

class Steppable s where
  step :: (Policy p) => s p -> s p
  getTimeS :: (Policy p) => s p -> Integer
  isTerminalS :: (Policy p) => s p -> Bool
  mkSteppable :: (Policy p) => (WorldView -> p) -> Int -> Environment -> StdGen -> s p

instance Steppable Scenario where
  step = stepScenario
  getTimeS = getTime
  isTerminalS (Scenario _ ws _ _) = isTerminal ws
  mkSteppable policyF nd env gen = mkScenario policyF nd env --don't really need the gen, this is clumsy

instance Steppable RandomScenario where
  step (RandomScenario gen scenario@(Scenario pol ws@(WorldState env info enStat) time hist)) = RandomScenario nextGen newScen
    where
      newScen = Scenario nextPol newWs (time + 1) newHist
      newHist = case nextMoves of
        [] -> hist
        na@(move : moves) -> (Snapshot na time) : hist
      (nextMoves, nextPol) = nextMove pol (toView droppedWs)
      newWs = updateState nextMoves droppedWs

      droppedWs = WorldState env info newEnStat
      newEnStat = if (dropADrone && (length enStat >= 2)) --don't ever drop the last drone because this is just to show adaptability
        then (tail $ shuffle shuffleGen enStat)
        else enStat
      dropADrone = roll <= 0.002 --hard coded probability
      (roll, nextGen) = randomR (0.0 :: Float, 1.0) rollGen
      (shuffleGen, rollGen) = split gen
  getTimeS (RandomScenario _ (Scenario _ _ time _)) = time
  isTerminalS (RandomScenario _ scenario) = isTerminalS scenario
  mkSteppable = mkRandomScenario


--advance the current scenario one time step
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

--should I add a time limit to this?
--run the scenario until its time hits the time limit, or until it finishes, whichever comes first
runScenario :: (Policy p) => Integer -> Scenario p -> (Bool, Scenario p)
runScenario timeLimit s = case (overTime || finished) of
                      False -> runScenario timeLimit $ stepScenario s
                      True -> (finished, s)
  where
    overTime = getTime s >= timeLimit
    finished = isTerminal $ getWorldState s

--time if success, Nothing if failure
timeScenarioRun :: (Policy p) => Integer -> Scenario p -> Maybe Integer
timeScenarioRun timeLimit s =
  if success
    then Just $ getTime afterRun
    else Nothing

    where
     (success, afterRun) = runScenario timeLimit s

--fullRun initializes a scenario from an environment and number of drones and sets it to run to completion with a time limit
fullRun :: (Policy p) => Integer -> Int -> (WorldView -> p) -> Environment -> (Bool, Scenario p)
fullRun timeLimit numDrones policyF environment = runScenario timeLimit scenario
  where
    scenario = mkScenario policyF numDrones environment

runSteppable :: (Policy p, Steppable s) => Integer -> s p -> (Bool, s p)
runSteppable timeLimit s = case (overTime || finished) of
  False -> runSteppable timeLimit $ step s
  True -> (finished, s)
  where
    overTime = getTimeS s >= timeLimit
    finished = isTerminalS s

fullSteppableRun :: (Policy p, Steppable s) => Integer -> Int -> StdGen -> (WorldView -> p) -> Environment -> (Bool, s p)
fullSteppableRun timeLimit numDrone gen policyF environment = runSteppable timeLimit s
  where
    s = mkSteppable policyF numDrone environment gen

fullRandomScenarioRun :: (Policy p) => Integer -> Int -> StdGen -> (WorldView -> p) -> Environment -> (Bool, RandomScenario p)
fullRandomScenarioRun = fullSteppableRun

--in this case, the moveHistory is what is coming next
data ScenarioReplay = ScenarioReplay WorldState Integer MoveHistory
  deriving Show

createReplay :: Scenario p -> ScenarioReplay
createReplay sc@(Scenario _ ws@(WorldState env info ensembleStat) _ hist) = ScenarioReplay startWS 0 (reverse hist)
  where
    startWS = initializeWorldState (length ensembleStat) env

createReplayWithDropout :: Int -> Scenario p -> ScenarioReplay
createReplayWithDropout nDrones sc@(Scenario _ ws@(WorldState env info ensembleStat) _ hist) = ScenarioReplay startWS 0 (reverse hist)
  where
    startWS = initializeWorldState nDrones env

--move the replay foreward one time step and get the new worldstate that resulted from that
advanceReplay :: ScenarioReplay -> ScenarioReplay
advanceReplay srp@(ScenarioReplay ws time hist) =
  if (isTerminal ws) then srp
    else case hist of
           [] -> ScenarioReplay (updateState [] ws) (time + 1) hist
           (snap : snaps) -> ScenarioReplay newWs (time + 1) newHist
  where
    (newWs, newHist) = if (nextMoveTime == time)
              then (updateState (getCommands nextSnap) ws, tail hist)
              else (updateState [] ws, hist)

    nextMoveTime = getTimeStamp nextSnap
    nextSnap = head hist

advanceUntilTime :: Integer -> ScenarioReplay -> ScenarioReplay
advanceUntilTime t sr@(ScenarioReplay ws time hist) =
  if (time >= t || isTerminal ws) then sr
    else advanceUntilTime t $ advanceReplay sr

-- below are some utlilty functions to help start and end a scenario --------------------------------------------------------------------

--takes an environment, a number of drones to spawn, and creates a WorldState representing a completely unexplored scenario
initializeWorldState :: Int -> Environment -> WorldState
initializeWorldState i env = WorldState env info enStat
  where
    info = EnvView.initializeInfo env
    minPosition = Set.findMin . Map.keysSet . getMap $ env--this might fail for empty environments? --type of this is Position
    enStat = fmap numberDrone [1 .. i]
    numberDrone n = (DroneID n, Unassigned (DronePos minPosition Low))

isTerminal :: WorldState -> Bool
isTerminal ws = isCompleteInfo (getInfo ws)

isCompleteInfo :: EnvironmentInfo -> Bool
isCompleteInfo envInfo = getAll $ foldMap (All . EnvView.isFullyObserved) envInfo
