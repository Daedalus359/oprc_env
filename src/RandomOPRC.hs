module RandomOPRC where

import RandomAgent
import Scenario
import Env

import System.Random as Random
import Control.Monad

randPolicy :: IO (RandomPolicy)
randPolicy = fmap RandomPolicy newStdGen

randGen :: IO StdGen
randGen = newStdGen

randAgentRunTime :: Integer -> Integer -> Environment -> StdGen -> Maybe Integer
randAgentRunTime numDrones timeLimit env gen = timeScenarioRun timeLimit scenario
  where scenario = mkScenario (RandomPolicy gen) numDrones env

randAgentRunTimes :: Int -> Integer -> Integer -> Environment -> IO [Maybe Integer]
randAgentRunTimes numReps numDrones timeLimit env = (fmap . fmap) timedRun $ replicateM numReps newStdGen
  where timedRun = randAgentRunTime numDrones timeLimit env

averageRunTime :: [Maybe Integer] -> Maybe Integer
averageRunTime mbis = (fmap $ \x -> div x len) . (fmap $ foldr (+) 0) . sequenceA $ mbis
  where len = toInteger $ length mbis