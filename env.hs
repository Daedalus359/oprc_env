--Levels of Scrutiny that may be required for a patch in the search space
data DetailReq = Close | Far
  deriving Show

newtype XCoord = XPos Int
  deriving Show
newtype YCoord = YPos Int
  deriving Show

--location for a patch (horizontal information only)
data Position = Position XCoord YCoord

--a patch is the a single spot in the map
data Patch = Patch Position DetailReq

--pretty printing for patches
instance Show Patch where
  show (Patch position observationLevel) = concat ["Patch:\n", 
                                                    "\t", show position, "\n", 
                                                    "\tScrutiny Needed: ", show observationLevel]

--pretty printing for positions
instance Show Position where
  show (Position (XPos xc) (YPos yc)) = concat ["Position: (", show xc, ", ", show yc, ")"]

data ObservationLevel = Unseen | Classified | FullyObserved
  deriving Show

--function to tell if a coordinate is inside the boundary of the map
inBounds :: Position -> Bool
inBounds = undefined

--function to map a coordinate to the information known about a patch at that position
--infoState :: Position -> Maybe

--Heights that the drone can fly at
data Altitude = High | Low
  deriving Show