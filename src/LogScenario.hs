module LogScenario where

{-# LANGUAGE OverloadedStrings #-}

{-
Basically, this makes a variant of the scenario management code (i.e. runScenario) that logs information about the WorldState at each time step
Then, it transforms the minimal information associated with each time step into a "row" that includes some derived quantities
-}

--A lot of this ONLY WORKS WITH FOUR DRONES

{-
TODO:
1. write blueFrac
2. use blueFrac to fill f_1 through f_4 in mkAttractorRow
3. compute distances between drones for mkAttractorRow
4. refer to the todo in AnomalyData.hs
-}

import qualified Data.Csv as Csv
import Scenario
import WorldState
import Policy
import EnvView
import Env
import qualified Data.ByteString.Lazy as BS
import qualified Data.Binary as Bin
import Data.List
import Drone

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
mkAttractorRow wsMoment@(WorldStateMoment time ws@(WorldState env info enStat)) = undefined
{-
  AttractorLogRow
    x_1
    y_1
    a_1
    f_1

    x_2
    y_2
    a_2
    f_2

    x_3
    y_3
    a_3
    f_3

    x_4
    y_4
    a_4
    f_4

    d_1_2
    d_1_3
    d_1_4
    d_2_3
    d_2_4
    d_3_4

    d_med
    time
  where
    (x_1, y_1, a_1, p_1) = droneStats $ snd $ enStat !! 0
    (x_2, y_2, a_2, p_2) = droneStats $ snd $ enStat !! 1
    (x_3, y_3, a_3, p_3) = droneStats $ snd $ enStat !! 2
    (x_4, y_4, a_4, p_4) = droneStats $ snd $ enStat !! 3

    [f1, f2, f3, f4] = fmap (blueFrac env) [p_1, p_2, p_3, p_4]

    d_med = (!! 3) $ sort [d_1_2, d_1_3, d_1_4, d_2_3, d_2_4, d_3_4]

-}

blueFrac :: Environment -> Position -> Float
blueFrac = undefined

droneStats :: DroneStatus -> (Float, Float, Float, Position)
droneStats (Unassigned (DronePos pos@(Position x y) alt)) = (fromIntegral x, fromIntegral y, floatAlt, pos)
  where
    floatAlt = case alt of
      High -> 1.0
      Low -> 0.0
droneStats (Assigned action (DronePos pos@(Position x y) alt)) = (fromIntegral x, fromIntegral y, floatAlt, pos)
  where
    floatAlt = case alt of
      High -> 1.0
      Low -> 0.0
droneStats (Acting action stepsRemainingI dpos@(DronePos pos@(Position x y) alt)) = (x_mixed, y_mixed, alt_mixed, pos)
  where
    stepsRemaining = fromIntegral stepsRemainingI
    (newPos, progressFrac) = case action of
      MoveCardinal cardinalDir -> (DronePos (hopFrom pos $ deltas cardinalDir) alt, stepsRemaining / 10.0)
      MoveIntercardinal icDir -> (DronePos (hopFrom pos $ deltas icDir) alt, stepsRemaining / 14.0)
      MoveVertical Ascend -> case alt of
        High -> (dpos, 1.0)
        Low -> ((DronePos (Position x y) High), stepsRemaining / 10.0)
      MoveVertical Descend -> case alt of
        High -> ((DronePos (Position x y) Low), stepsRemaining / 10.0)
        Low -> (dpos, 1.0)
      Hover -> (dpos, 1.0)
    (x_init, y_init, alt_init, _) = droneStats (Unassigned dpos)
    (x_final, y_final, alt_final, _) = droneStats (Unassigned newPos)
    x_mixed = x_final * progressFrac + x_init * (1 - progressFrac)
    y_mixed = y_final * progressFrac + y_init * (1 - progressFrac)
    alt_mixed = alt_final * progressFrac + alt_init * (1 - progressFrac)



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