module SampleVals where

import Env
import Drone
import Ensemble
import EnvGen
import EnvView
import WorldState
import Scenario
import RandomAgent
import Policy
import ShapeSweepAgent
import GraphOPRC
import RandomOPRC
import SpanningTreeAgent

import ParseOPRC
import Text.Trifecta

import PrettyOPRC
import Data.Text.Prettyprint.Doc
import Data.Text.Prettyprint.Doc.Util

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

import Control.Monad (forever)
import System.Exit
import System.Random
import Control.Applicative

--Datatypes defined in Env

xc :: Env.XCoord
xc = 0

yc :: Env.YCoord
yc = 0

pos :: Env.Position
pos = Position xc yc

close :: Env.DetailReq
close = Close

pat :: Env.Patch
pat = Patch close

high :: Env.Altitude
high = High

hopNW :: Env.Hop
hopNW = (-1, 1)

north :: Env.CardinalDir
north = North

sw :: Env.IntercardinalDir
sw = SW

--setup extra patch for environment
pos2 :: Env.Position
pos2 = Position (xc + 1) yc

pat2 :: Env.Patch
pat2 = Patch Far

env :: Env.Environment
env  = Environment $ Map.fromList [(pos, pat), (pos2, pat2)]

parseEnv3 :: IO (Result Env.Environment)
parseEnv3 = fmap (parseString parseEnvironment mempty) $ readFile "./test/environments/3.env"

parsedWs :: IO (Result WorldState)
parsedWs = (fmap . fmap) (initializeWorldState 2) parseEnv3

parseEnvFilePath :: String -> IO (Result Env.Environment)
parseEnvFilePath path = fmap (parseString parseEnvironment mempty) $ readFile path

--this can fail
envFromFilePath :: String -> IO Env.Environment
envFromFilePath path = do
  result <- parseEnvFilePath path
  case result of
    Success env -> return env
    Failure _ -> exitFailure

parseEnvNum :: Integer -> IO (Result Env.Environment)
parseEnvNum i = fmap (parseString parseEnvironment mempty) $ readFile fileStr
  where fileStr = "./test/environments/" ++ (show i) ++ ".env"

liftToWS :: Int -> IO (Result Env.Environment) -> IO (Result WorldState)
liftToWS i envIO = (fmap . fmap) (initializeWorldState i) envIO

dumpParseFailure :: IO (Result a) -> IO a
dumpParseFailure ioRa = do
  resA <- ioRa
  case resA of
    Success v -> return v
    Failure _ -> do
      putStrLn "Parsing failed to load value"
      exitSuccess

dumpParseFailure2 :: (a -> IO ()) -> IO (Result a) -> IO ()
dumpParseFailure2 f possibleVal = do
  pVal <- possibleVal --Result val, comes out of IO shell
  case pVal of
    Success v -> f v
    Failure _ -> do --consider passing through the error message from the Failure itself
      putStrLn "Parsing failed to load value"

--just the shape of the environment
footprint :: Env.Footprint
footprint = Map.keysSet $ toMap env

footprint2 :: Env.Footprint
footprint2 = Set.fromList $ zipWith Position xs ys
  where
    (xs, ys) = unzip [(0, 0), (0, 3), (1,0), (1,1), (1,2), (1,3), (2,3)]


--Currently no datatypes defined in MoveCosts

--Datatypes defined in Drone

dronePos :: Drone.DronePosition
dronePos = DronePos pos high

ascend = Ascend :: Drone.VerticalDirection

moveUp = MoveVertical ascend :: Drone.Action

fiveSteps = 5 :: Drone.StepsRemaining

workingUp = Acting moveUp fiveSteps dronePos :: Drone.DroneStatus

--Datatypes defined in Ensemble
drone1 = DroneID 1 :: Drone.Drone
drone2 = DroneID 2 :: Drone.Drone

drones = [drone1, drone2] :: Ensemble.DroneList

ensembleStatus = [(drone1, workingUp), (drone2, (Acting (MoveVertical Descend) 4 dronePos))] :: Ensemble.EnsembleStatus 

--Datatypes defined in EnvView

knownClose = Classified Close :: EnvView.PatchInfo

envInfo = Map.fromList [(pos, knownClose), (pos2, Unseen)] :: EnvView.EnvironmentInfo

--Datatypes defined in WorldState

worldState = WorldState env envInfo ensembleStatus :: WorldState.WorldState

--make an example policy
--put example of NextActions in under Ensemble

quitIfTerminal :: WorldState -> IO ()
quitIfTerminal ws =
  if (isTerminal ws) then
    do putStrLn "The scenario has ended. All patches have been observed."
       exitSuccess
  else return ()

getNewCommandTerm :: IO NextActions
getNewCommandTerm = do
  putStrLn "Enter the NextActions to command, e.g. [(1, Hover), (2, MoveIntercardinal NE)] :"
  naResult <- fmap (parseString parseNextActions mempty) getLine --get a string representing the user's next commands to the ensemble
  case naResult of
    Failure _ -> do
      putStrLn "Unable to parse previous input, try again."
      getNewCommandTerm
    Success na -> return na

--lets the user control the ensemble interactively by specifying nextactions at each time step
manualControl :: WorldState -> IO ()
manualControl ws = forever $ do
  putDocW 80 (pretty ws)
  quitIfTerminal ws
  putStrLn "The following drones are Unassigned and will respond to a NextActions instruction"
  print $ Ensemble.needsCommand (getEnsemble ws)
  na <- getNewCommandTerm
  manualControl (updateState na ws)

randFiltPolicy :: IO (RandomFilteredPolicy)
randFiltPolicy = fmap RandomFilteredPolicy newStdGen

lsPolicy :: LowSweepPolicy
lsPolicy = LowSweepPolicy []

fullScenarioWithOutput :: Policy p => IO (WorldView -> p) -> Int -> Integer -> Integer -> IO (Scenario p)
fullScenarioWithOutput ioPol nDrones envNum timeLimit = do
    putStrLn "Running scenario..."
    (finished, scenario) <- liftA2 (Scenario.fullRun timeLimit nDrones) (ioPol) (dumpParseFailure $ parseEnvNum envNum)-- IO (Bool, Scenario RandomPolicy)
    putStrLn "Environment explored? - "
    print finished
    --putStrLn "Final worldState: "
    --putDocW 80 (pretty $ Scenario.getWorldState scenario)
    putStrLn "Final time: "
    print (Scenario.getTime scenario)
    --putStrLn "Move History: "
    --putDocW 80 (vsep $ fmap pretty $ Scenario.getHist scenario)
    return scenario

fileNameScenarioWithOutput :: Policy p => IO (WorldView -> p) -> Int -> String -> Integer -> IO (Scenario p)
fileNameScenarioWithOutput ioPol nDrones path timeLimit = do
  putStrLn "Running scenario..."
  (finished, scenario) <- liftA2 (Scenario.fullRun timeLimit nDrones) (ioPol) (dumpParseFailure $ parseEnvFilePath path)-- IO (Bool, Scenario RandomPolicy)
  putStrLn "Environment explored? - "
  print finished
  --putStrLn "Final worldState: "
  --putDocW 80 (pretty $ Scenario.getWorldState scenario)
  putStrLn "Final time: "
  print (Scenario.getTime scenario)
  --putStrLn "Move History: "
  --putDocW 80 (vsep $ fmap pretty $ Scenario.getHist scenario)
  return scenario

fileNameFirstSteps :: (Pretty p, Policy p) => IO (WorldView -> p) -> Int -> String -> Integer -> IO (Scenario p)
fileNameFirstSteps ioPol nDrones path timeLimit = do
  putStrLn "Initialization Step:"
  scenario <- liftA3 (Scenario.mkScenario) ioPol (return nDrones) (dumpParseFailure $ parseEnvFilePath path)
  putStrLn "One step policy: "
  putDocW 80 $ pretty $ getPolicy $ stepScenario $ scenario
  putStrLn "scenario gets spit out below in ghci..."
  return scenario

firstStepsWithOutput :: IO ()
firstStepsWithOutput = do
  scenario <- liftA3 (Scenario.mkScenario) (fmap const randPolicy) (return 1) (dumpParseFailure $ parseEnvNum 4)
  putStrLn "Unstepped time: "
  print (Scenario.getTime scenario)
  putStrLn "Unstepped move hist"
  print (vsep $ fmap pretty $ Scenario.getHist scenario)
  putStrLn "One step time"
  print (Scenario.getTime $ stepScenario scenario)
  putStrLn "One step hist"
  print (vsep $ fmap pretty $ Scenario.getHist $ stepScenario scenario)
  putStrLn "Two step time"
  print (Scenario.getTime $ stepScenario $ stepScenario scenario)
  putStrLn "Two step hist"
  putDocW 80 (vsep $ fmap pretty $ Scenario.getHist $ stepScenario $ stepScenario scenario)
  --putStrLn "Three step time"
  --print (Scenario.getTime $ stepScenario $ stepScenario $ stepScenario scenario)
  --putStrLn "Three step hist"
  --print (Scenario.getHist $ stepScenario $ stepScenario $ stepScenario scenario)
  return ()

sampleAStar :: IO (Maybe Path)
sampleAStar = aStarStandardPenalty Low <$> (fmap initializeInfo $ dumpParseFailure $ parseEnvNum 2) <*> (return mkManhattanHeuristic) <*> (return (Position 0 0)) <*> (return (Position 1 1))

threeStepsOfOutput :: (Policy p, Pretty p) => IO (WorldView -> p) -> Int -> Integer -> Integer -> IO ()
threeStepsOfOutput pF numDrones envNum timeLimit = do
  scenario <- Scenario.mkScenario <$> pF <*> (pure numDrones) <*> (dumpParseFailure $ parseEnvNum envNum)
  oneStep <- return $ stepScenario scenario
  (finished100, sc300) <- return $ runScenario 300 oneStep

  putStrLn "Unstepped time: "
  print (Scenario.getTime scenario)
  putStrLn "Unstepped policy: "
  putDocW 80 (pretty $ Scenario.getPolicy scenario)
  --putStrLn "Unstepped move hist"
  --print (vsep $ fmap pretty $ Scenario.getHist scenario)
  putStrLn "Unstepped WorldState"
  putDocW 80 (pretty $ Scenario.getWorldState scenario)

  --worldView <- return $ toView $ Scenario.getWorldState scenario
  --envInfo <- return $ getView worldView

  --putStrLn "One step time"
  --print (Scenario.getTime oneStep)
  --putStrLn "One step policy"
  --putDocW 80 (pretty $ Scenario.getPolicy oneStep)
  --putStrLn "One step hist"
  --print (vsep $ fmap pretty $ Scenario.getHist oneStep)
  --putStrLn "One step WorldState"
  --putDocW 80 (pretty $ Scenario.getWorldState oneStep)
  --putStrLn ""

  if finished100
    then putStrLn "Scenario finished within 300 steps"
    else do
           putStrLn "300 step policy"
           putDocW 80 (pretty $ Scenario.getPolicy sc300)

  return ()

  --putStrLn "Two step time"
  --print (Scenario.getTime $ stepScenario $ stepScenario scenario)
  --putStrLn "Two step hist"
  --putDocW 80 (vsep $ fmap pretty $ Scenario.getHist $ stepScenario $ stepScenario scenario)

  --putStrLn "Three step time"
  --print (Scenario.getTime $ stepScenario $ stepScenario $ stepScenario scenario)
  --putStrLn "Three step hist"
  --print (Scenario.getHist $ stepScenario $ stepScenario $ stepScenario scenario)

kmp :: IO (WorldView -> KMeansLowPolicy)
kmp = initializeKMP 10 <$> randGen

--sample values:
fromFPBernoulli70 :: Footprint -> EnvGen
fromFPBernoulli70 fp = EnvGen (\gen -> bernoulliEnv (BernoulliGen 0.7 gen) fp)

lstp :: IO (WorldView -> LowSpanningTreePolicy)
lstp = return initializeLSTP

lkmstp :: IO (WorldView -> LowKMeansSpanningTreePolicy)
lkmstp = initializeLKMSTP 10 <$> newStdGen