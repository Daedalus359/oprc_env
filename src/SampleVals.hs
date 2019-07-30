module SampleVals where

import Env
import Drone
import Ensemble
import EnvView
import WorldState

import ParseOPRC
import Text.Trifecta

import PrettyOPRC
import Data.Text.Prettyprint.Doc
import Data.Text.Prettyprint.Doc.Util

import qualified Data.Map as Map
import qualified Data.Set as Set

import Control.Monad (forever)
import System.Exit

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

parseEnvNum :: Integer -> IO (Result Env.Environment)
parseEnvNum i = fmap (parseString parseEnvironment mempty) $ readFile fileStr
  where fileStr = "./test/environments/" ++ (show i) ++ ".env"

liftToWS :: Integer -> IO (Result Env.Environment) -> IO (Result WorldState)
liftToWS i envIO = (fmap . fmap) (initializeWorldState i) envIO

dumpFailure :: (WorldState -> IO ()) -> IO (Result WorldState) -> IO ()
dumpFailure f possibleWS = do
  pWS <- possibleWS --Result WorldState
  case pWS of
    Success ws -> f ws
    Failure _ -> do --consider passing through the error message from the Failure itself
      putStrLn "Parsing failed before worldstate was created"

--just the shape of the environment
footprint :: Env.Footprint
footprint = Map.keysSet $ toMap env


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


--lets the user control the ensemble interactively by specifying nextactions at each time step
manualControl :: WorldState -> IO ()
manualControl ws = forever $ do
  putDocW 80 (pretty ws)
  quitIfTerminal ws
  putStrLn "The following drones are Unassigned and will respond to a NextActions instruction"
  print $ needsCommand (getEnsemble ws)
  putStrLn "Enter the NextActions to command, e.g. [(1, Hover), (2, MoveIntercardinal NE)] :"
  naResult <- fmap (parseString parseNextActions mempty) getLine --get a string representing the user's next commands to the ensemble
  case naResult of
    Failure _ -> manualControl ws
    Success na -> manualControl (updateState na ws)