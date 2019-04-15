module Env where --The simulated world

import qualified Data.Map as Map

--location for a patch (horizontal information only)
type XCoord = Int
type YCoord = Int

data Position = Position XCoord YCoord
  deriving Eq

instance Show Position where
  show (Position xc yc) = concat ["Position: (", show xc, ", ", show yc, ")"]

instance Ord Position where
  compare (Position x1 y1) (Position x2 y2)
    | x1 /= x2 = compare x1 x2
    | otherwise = compare y1 y2 

--Levels of Scrutiny that may be required
data DetailReq = Close | Far
  deriving Show

--a patch is a single spot in the map
data Patch = Patch DetailReq

--pretty printing for patches
instance Show Patch where
  show (Patch detailReq) = concat ["Patch:\n", "\tScrutiny Needed: ", show detailReq]

--function to tell if a coordinate is inside the boundary of the map
inBounds :: Position -> Bool
inBounds = undefined

--Heights that the drone can fly at
data Altitude = High | Low
  deriving Show

--type synonym representing a change in position
type Hop = (Int, Int)

--compass directions
class Direction d where
  deltas :: d -> Hop

data CardinalDir = North | South | East | West
  deriving (Show, Eq)

instance Direction CardinalDir where
  deltas North = (0, 1)
  deltas South = (0, -1)
  deltas East = (1, 0)
  deltas West = (-1, 0)

data IntercardinalDir = NE | SE | NW | SW
  deriving (Show, Eq)

instance Direction IntercardinalDir where
  deltas NE = (1, 1)
  deltas SE = (1, -1)
  deltas NW = (-1, 1)
  deltas SW = (-1, -1)

-- "NeighborTo North (3,4) = (3,5)""
neighborTo :: Direction d => d -> Position -> Position
neighborTo dir (Position x y) = Position (x + (fst (deltas dir))) (y + (snd (deltas dir)))

--The "world" is a colection of patches at different positions
type World = Map.Map Position Patch

--MOVE
data ObservationLevel = Unseen | Classified | FullyObserved
  deriving Show

--function to map a coordinate to the information known about a patch at that position
----infoState :: Position -> Maybe
