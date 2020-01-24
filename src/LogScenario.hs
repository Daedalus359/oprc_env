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

data AttractorLogRow =
  AttractorLogRow {
    d1PosX :: Float
  , d1PosY :: Float
  , d1Alt :: Float
  , d1BlueFrac :: Float

  , d2PosX :: Float
  , d2PosY :: Float
  , d2Alt :: Float
  , d2BlueFrac :: Float

  , d3PosX :: Float
  , d3PosY :: Float
  , d3Alt :: Float
  , d3BlueFrac :: Float

  , d4PosX :: Float
  , d4PosY :: Float
  , d4Alt :: Float
  , d4BlueFrac :: Float

  , dist1_2 :: Float
  , dist1_3 :: Float
  , dist1_4 :: Float
  , dist2_3 :: Float
  , dist2_4 :: Float
  , dist3_4 :: Float

  , dist_median :: Float
  }