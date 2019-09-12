module MoveCosts where

import Env

class Costed c where
  cost :: c -> Int

--movement in a cardinal direction costs 10
instance Costed CardinalDir where
  cost _ = 10

instance Costed IntercardinalDir where
  cost _ = 14 --roughly equals cost of a cardinal dir * sqrt(2), write a test for this eventually

neighborsOfWithCosts :: Position -> [(Position, Int)]
neighborsOfWithCosts pos = neighborF <*> [pos]
  where
    neighborF = cardinalNeighborF ++ intercardinalNeighborF

    cardinalNeighborF = (fmap (\dir -> \pos -> (neighborTo dir pos, straightCost)) [North, South, East, West])
    intercardinalNeighborF = (fmap (\dir -> \pos -> (neighborTo dir pos, diagCost)) [NE, SE, NW, SW])

    straightCost = cost (undefined :: CardinalDir)
    diagCost = cost (undefined :: IntercardinalDir)

inBoundsNeighborsOfWithCosts :: Footprint -> Position -> [(Position, Int)]
inBoundsNeighborsOfWithCosts fp pos = filter (\(p, c) -> inBounds fp p) $ neighborsOfWithCosts pos