module Env where

--Levels of Scrutiny that may be required for a patch in the search space
data DetailReq = Close | Far
  deriving Show

type XCoord = Int
type YCoord = Int

--location for a patch (horizontal information only)
data Position = Position XCoord YCoord

--pretty printing for positions
instance Show Position where
  show (Position xc yc) = concat ["Position: (", show xc, ", ", show yc, ")"]

data ObservationLevel = Unseen | Classified | FullyObserved
  deriving Show

--a patch is the a single spot in the map
data Patch = Patch Position DetailReq

data World = World [Patch]

instance Show World where
  --make this into an actual visualization later
  show (World pats) = concat["World: ", (show pats)]

--pretty printing for patches
instance Show Patch where
  show (Patch position observationLevel) = concat ["Patch:\n", 
                                                    "\t", show position, "\n", 
                                                    "\tScrutiny Needed: ", show observationLevel]

data SearchSpace = MkSpace [Patch]

--function to tell if a coordinate is inside the boundary of the map
inBounds :: Position -> Bool
inBounds = undefined

--function to map a coordinate to the information known about a patch at that position
--infoState :: Position -> Maybe

--Heights that the drone can fly at
data Altitude = High | Low
  deriving Show

--type synonym representing a chainge in position
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

-- "position to North (3,4) = (3,5)""
neighborTo :: Direction d => d -> Position -> Position
neighborTo dir (Position x y) = Position (x + (fst (deltas dir))) (y + (snd (deltas dir)))
