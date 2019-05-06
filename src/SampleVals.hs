module SampleVals where

import Env
import Drone
import Ensemble

import qualified Data.Map as Map
import qualified Data.Set as Set


--Datatypes defined in Env

xc :: Env.XCoord
xc = 5

yc :: Env.YCoord
yc = (-3)

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
env  = Map.fromList [(pos, pat), (pos2, pat2)]

--just the shape of the environment
footprint :: Env.Footprint
footprint = Map.keysSet env


--Currently no datatypes defined in MoveCosts

--Datatypes defined in Drone

dronePos :: Drone.DronePosition
dronePos = DronePos pos high

ascend = Ascend :: Drone.VerticalDirection

moveUp = MoveVertical ascend :: Drone.Action

fiveSteps = 5 :: Drone.StepsRemaining

workingUp = Acting moveUp fiveSteps :: Drone.DroneStatus

--Datatypes defined in Ensemble
drone1 = DroneID 1 :: Ensemble.Drone

--make an example world
--make an example policy