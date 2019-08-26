module EnvGen where

import Env

import System.Random
import Control.Monad
import qualified Data.Set as Set
import qualified Data.Map.Strict as Map

class EnvGen e where
  mkEnv :: e -> StdGen -> Environment

--the double should be a valid probability, i.e. between 0 and 1
data BernoulliGen = BernoulliGen Double StdGen

patchBernoulli :: BernoulliGen -> (Patch, StdGen)
patchBernoulli bg@(BernoulliGen threshold gen) = 
  if (val >= threshold)
    then (Patch Far, gen2)
    else (Patch Close, gen2)
  where (val, gen2) = randomR (0, 1) gen

bernoulliEnv :: BernoulliGen -> Footprint -> Environment
bernoulliEnv bg fp = Environment $ Map.fromList $ zip posList patList
  where
    patList = bernoulliList bg
    posList = Set.toList fp

bernoulliList :: BernoulliGen -> [Patch]
bernoulliList bg@(BernoulliGen threshold gen) = fmap Patch $ fmap toDR $ randomRs (0, 1 :: Double) gen
  where
    toDR x =
      if (x >= threshold)
        then Far
        else Close

randomFootprint :: StdGen -> Int -> Int -> Int -> Int -> Int -> Maybe Footprint
randomFootprint gen varLimit xMin xMax yMin yMax =
  if (xMin < xMax && yMin < yMax)
    then maybeFootprint
    else Nothing

  where
    maybeFootprint = fmap Set.fromList allPatches

    allPatches = fmap join $ sequence $ zipWith fillSpace leftFrontier rightFrontier

    rightFrontier = frontier lowCorner lowRightBridge rightCorner highRightBridge highCorner
    leftFrontier = frontier lowCorner lowLeftBridge leftCorner highLeftBridge highCorner

    highRightBridge = bridge urbGen varLimit Lft rightCorner highCorner
    lowRightBridge = bridge lrbGen varLimit Lft lowCorner rightCorner
    highLeftBridge = bridge ulbGen varLimit Rt leftCorner highCorner
    lowLeftBridge = bridge llbGen varLimit Rt lowCorner leftCorner

    (lrbGen, urbGen) = split rbGen
    (llbGen, ulbGen) = split lbGen
    (lbGen, rbGen) = split gen3

    [lowCorner, highCorner] = zipWith Position xValsRandom [yMin, yMax]
    [leftCorner, rightCorner] = zipWith Position [xMin, xMax] yValsRandom
    yValsRandom = randomRs (yMin, yMax) yvGen
    xValsRandom = randomRs (xMin, xMax) xvGen

    (xvGen, yvGen) = split gen2
    (gen2, gen3) = split gen
  --generate 4 boundary Positions by adding random y values to the two x bounds and vice versa
  --compute the left frontier by stitching together

fillSpace :: Position -> Position -> Maybe [Position]
fillSpace leftEnd@(Position xl yl) rightEnd@(Position xr yr) =
  if (yl == yr)
    then Just $ fmap (\x -> Position x yl) [xl .. xr]
    else Nothing

--does not actually preserve all of the provided positions, but shouldn't be a problem if it's only called with bridges generated from the positions
frontier :: Position -> [XCoord] -> Position -> [XCoord] -> Position -> [Position]
frontier atYMin@(Position x0 y0) bridge1 atXBound@(Position x1 y1) bridge2 atYMax@(Position x2 y2) = zipWith Position xs ys
  where
    xs = [x0] ++ bridge1 ++ [x1] ++ bridge2 ++ [x2]
    ys = [y0 + 1 .. ]
  --make zip pairs with the corresponding y values for bridge1 and bridge2

data ProtectDir = Rt | Lft

--need to analyze the behavior more if the y values differ by 0
--run fmap (clamp lowerBound upperBound) on the result of this if you have bounds to respect
bridge :: StdGen -> Int -> ProtectDir -> Position -> Position -> [XCoord]
bridge gen varLimit pDir p1@(Position x1 y1) p2@(Position x2 y2) = fmap protF unprotected



  where

    protF =
      case pDir of
        Rt -> min (max x1 x2)
        Lft -> max (min x1 x2)

    unprotected = if (numVals == 0) then [] else tail $ scanl (+) x1 deltas

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