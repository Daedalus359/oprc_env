module LogScenario where

{-
Basically, this makes a variant of the scenario management code (i.e. runScenario) that logs information about the WorldState at each time step
Then, it transforms the minimal information associated with each time step into a "row" that includes some derived quantities
-}

import qualified Data.Csv as Csv
import Scenario
import WorldState
import Policy
import EnvView
import Env

data WorldStateMoment =
  WorldStateMoment {
    time :: Integer
  , ws :: WorldState
  }
  deriving (Eq, Show)

type ScenarioLog = [WorldStateMoment]

logRun :: (Policy p) => Integer -> Scenario p -> ScenarioLog
logRun timeLimit s = case (overTime || finished) of
  False -> wsm : (logRun timeLimit $ stepScenario s)
  True -> [wsm]

  where
    nowTime = getTime s
    nowWS = getWorldState s

    wsm :: WorldStateMoment
    wsm = WorldStateMoment nowTime nowWS

    overTime = nowTime >= timeLimit
    finished = isTerminal nowWS

--same as the above, but manages scenario initialization as well
fullLogRun :: (Policy p) => Integer -> Int -> (WorldView -> p) -> Environment -> ScenarioLog
fullLogRun timeLimit numDrones policyF environment = logRun timeLimit scenario
  where
    scenario = mkScenario policyF numDrones environment

namesRow = Csv.record $ fmap Csv.toField ["Drone1 Position", "Drone2 Position", "Drone3 Position", "Drone4 Position"]

data AttractorLogRow =
  AttractorLogRow {
    name1 :: String
  }