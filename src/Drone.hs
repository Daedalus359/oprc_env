module Drone where -- the simulation of one drone

import Env
import MoveCosts

data DronePosition = DronePos Env.Position Env.Altitude
  deriving (Eq, Show)

data VerticalDirection = Ascend | Descend
  deriving (Eq, Show)

data Action =
    MoveCardinal Env.CardinalDir
  | MoveIntercardinal Env.IntercardinalDir
  | MoveVertical VerticalDirection
  | Hover
  deriving (Eq, Show)

--add a function like hopFrom for vertical positions if I eventually have more than two

movedBy :: Action -> DronePosition -> DronePosition
movedBy Hover dPos = dPos
movedBy (MoveCardinal dir) (DronePos pos alt)  = DronePos (hopFrom pos (deltas dir)) alt
movedBy (MoveIntercardinal dir) (DronePos pos alt) = DronePos (hopFrom pos (deltas dir)) alt
movedBy (MoveVertical vDir) (DronePos pos alt)
  | vDir == Ascend = (DronePos pos High)
  | otherwise = (DronePos pos Low)

class Timed t where
  duration :: t -> Integer

instance Timed Action where
  duration Hover = 1
  duration (MoveCardinal _) = 10
  duration (MoveIntercardinal _ ) = 14
  duration (MoveVertical _ ) = 10

viewableFrom :: DronePosition -> [Position]
viewableFrom (DronePos pos Low) = [pos]
viewableFrom (DronePos pos High) = pos : (Env.neighborsOf pos)

type StepsRemaining = Integer

data DroneStatus =
  Unassigned DronePosition
  | Assigned Action DronePosition
  | Acting Action StepsRemaining DronePosition
  deriving (Eq, Show)