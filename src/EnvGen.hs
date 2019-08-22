module EnvGen where

import Env

import System.Random

class EnvGen e where
  mkEnv :: e -> StdGen -> Environment

--the double should be a valid probability, i.e. between 0 and 1
data BernoulliGen = BernoulliGen Double

patchBernoulli :: Double -> StdGen -> (Patch, StdGen)
patchBernoulli threshold gen = 
  if (val >= threshold)
    then (Patch Far, gen2)
    else (Patch Close, gen2)
  where (val, gen2) = randomR (0, 1) gen

randomFootprint :: StdGen -> Int -> Int -> Int -> Int -> Maybe Footprint
randomFootprint gen xMin xMax yMin yMax = undefined
  --check if bounds are compatible
  --generate 4 boundary Positions by adding random y values to the two x bounds and vice versa
  --compute the left frontier by stitching together

frontier :: Position -> [XCoord] -> Position -> [XCoord] -> Position -> [Position]
frontier atYMin bridge1 atXBound bridge2 atYMax = undefined
  --make zip pairs with the corresponding y values for bridge1 and bridge2


--run fmap (clamp lowerBound upperBound) on the result of this if you have bounds to respect
bridge :: StdGen -> Int -> Position -> Position -> [XCoord]
bridge gen varLimit p1@(Position x1 y1) p2@(Position x2 y2) = tail $ scanl (+) x1 deltas

  where

    deltas = zipWith (+) differences xShifts

    differences = (take rem $ repeat (avgDX + 1)) ++ (take (numVals - rem) $ repeat avgDX)
      --unshuffled - consider shuffling these for an improved approach
      --alternatively, being unshuffled might help move away from the x boundary quickly

    (avgDX, rem) = divMod (x2 - x1 - totalShift) numVals--average

    totalShift = sum xShifts
    xShifts = take numVals $ randomRs (-varLimit, varLimit) gen

    numVals = yDirFact * (y2 - y1) - 1--how many y values exist strictly between the two provided?
    yDirFact = if (y1 < y2) then 1 else (-1)

clamp :: Int -> Int -> Int -> Int
clamp lowBound highBound num =
  if num < lowBound
    then lowBound
    else if num > highBound
            then highBound
            else num