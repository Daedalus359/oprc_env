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

convexFootprint :: Int -> Int -> Int -> StdGen -> Maybe Footprint
convexFootprint width height numVertices gen =
  if (numVertices < 3 || (width < 2) || (height  < 2))
    then Nothing
    else undefined

makeConvex :: [Position] -> [Position]
makeConvex positions = undefined
  --find out which points are interior, filter those out
  --see if the list is the same as before give that back if so
  --otherwise add the difference in random points and run through again