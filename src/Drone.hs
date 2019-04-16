module Drone where -- the simulation of one drone

import Env
import MoveCosts

data DronePosition = DronePos Env.Position Env.Altitude
  deriving (Eq, Show)

data VerticalDirection = Ascend | Descend

data Action =
    MoveCardinal Env.CardinalDir
  | MoveInterCardinal Env.IntercardinalDir
  | MoveVertical VerticalDirection
  | Hover

class Timed t where
  duration :: t -> Integer

viewableFrom :: DronePosition -> [Position]
viewableFrom (DronePos pos Low) = [pos]
viewableFrom (DronePos pos High) = pos : (Env.neighborsOf pos)

data DroneStatus =
    Unassigned
  | Assigned Action
  | Acting {action :: Action, stepsRemaining :: Integer}
