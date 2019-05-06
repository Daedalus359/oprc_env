module Drone where -- the simulation of one drone

import Env
import MoveCosts

data DronePosition = DronePos Env.Position Env.Altitude
  deriving (Eq, Show)

data VerticalDirection = Ascend | Descend

data Action =
    MoveCardinal Env.CardinalDir
  | MoveIntercardinal Env.IntercardinalDir
  | MoveVertical VerticalDirection
  | Hover

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
    Unassigned
  | Assigned Action
  | Acting Action StepsRemaining