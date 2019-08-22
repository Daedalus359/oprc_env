module Env where --The simulated world

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

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
  deriving (Eq, Show)

--a patch is a single spot in the map
data Patch = Patch DetailReq
  deriving Eq

--pretty printing for patches
instance Show Patch where
  show (Patch detailReq) = concat ["Patch: ", "Scrutiny Needed: ", show detailReq]

--function to tell if a coordinate is inside the boundary of the map
inBounds :: Footprint -> Position -> Bool
inBounds footprint pos = Set.member pos footprint

--Heights that the drone can fly at
data Altitude = High | Low
  deriving (Show, Eq)

--type synonym representing a change in position
type Hop = (Int, Int)

hopFrom :: Position -> Hop -> Position
hopFrom (Position x y) (dX, dY) = (Position (x + dX) (y + dY))

getHop :: Position -> Position -> Hop
getHop (Position x1 y1) (Position x2 y2) = (x2 - x1, y2 - y1)

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

--the 8 positions which surround the input position
neighborsOf :: Position -> [Position]
neighborsOf pos = nl <*> [pos]
  where nl = (fmap neighborTo [North, South, East, West]) ++ (fmap neighborTo [NE, SE, NW, SW])

inBoundsNeighborsOf :: Footprint -> Position -> [Position]
inBoundsNeighborsOf fp pos = filter (inBounds fp) $ neighborsOf pos

--The "environment" is a colection of patches at different positions
newtype Environment = Environment {getMap :: (Map.Map Position Patch)}
  deriving Eq

instance Show Environment where
  show (Environment map) = "Environment: " ++ (show map)

toMap :: Environment -> Map.Map Position Patch
toMap (Environment map) = map

--The "Footprint" is a description of all points which are in bounds
type Footprint = Set.Set Position

--function to map a coordinate to the information known about a patch at that position
----infoState :: Position -> Maybe