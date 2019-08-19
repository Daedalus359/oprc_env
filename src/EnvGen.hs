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