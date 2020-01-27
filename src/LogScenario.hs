module LogScenario where

{-# LANGUAGE OverloadedStrings #-}

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
import qualified Data.ByteString.Lazy as BS
import qualified Data.Binary as Bin

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

mkAttractorData :: ScenarioLog -> [AttractorLogRow]
mkAttractorData = fmap mkAttractorRow

mkAttractorRow :: WorldStateMoment -> AttractorLogRow
mkAttractorRow wsMoment@(WorldStateMoment time ws) = undefined 

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

  , time_current :: Integer
  }

namesRow = 
  [ "Drone1_Pos_X"
  , "Drone1_Pos_Y"
  , "alt1"
  , "Drone1_Blue_Frac"

  , "Drone2_Pos_X"
  , "Drone2_Pos_Y"
  , "alt2"
  , "Drone2_Blue_Frac"

  , "Drone3_Pos_X"
  , "Drone3_Pos_Y"
  , "alt3"
  , "Drone3_Blue_Frac"

  , "Drone4_Pos_X"
  , "Drone4_Pos_Y"
  , "alt4"
  , "Drone4_Blue_Frac"

  , "Dist_1_2"
  , "Dist_1_3"
  , "Dist_1_4"
  , "Dist_2_3"
  , "Dist_2_4"
  , "Dist_3_4"

  , "Dist_Median"

  , "Time_Current"
  ]

instance Csv.ToNamedRecord AttractorLogRow where
  toNamedRecord (AttractorLogRow
    x1 y1 a1 f1 
    x2 y2 a2 f2 
    x3 y3 a3 f3 
    x4 y4 a4 f4 

    d12 d13 d14 d23 d24 d34 

    dmed time) =
    Csv.namedRecord $ zipWith Csv.namedField (fmap (BS.toStrict . Bin.encode) namesRow)
      [x1, y1, a1, f1, 
       x2, y2, a2, f2, 
       x3, y3, a3, f3, 
       x4, y4, a4, f4, 
       d12, d13, d14, d23, d24, d34, 
       dmed, (fromIntegral time)]

data SampleData = SampleData {alp :: Integer, bet :: Integer}

instance Csv.ToNamedRecord SampleData where
  toNamedRecord (SampleData a b) = Csv.namedRecord $ zipWith Csv.namedField (fmap (BS.toStrict . Bin.encode) ["alp", "bet"]) [a, b]