module MoveCosts where

import Env

class Costed c where
  cost :: c -> Int

--movement in a cardinal direction costs 10
instance Costed CardinalDir where
  cost _ = 10

instance Costed IntercardinalDir where
  cost _ = 14 --roughly equals cost of a cardinal dir * sqrt(2), write a test for this eventually
