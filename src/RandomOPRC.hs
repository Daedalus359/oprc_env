module RandomOPRC where

import RandomAgent

import System.Random as Random

randPolicy :: IO (RandomPolicy)
randPolicy = fmap RandomPolicy newStdGen

randGen :: IO StdGen
randGen = newStdGen

--randAgentRunTimes :: StdGen -> Integer -> Integer -> Integer -> Environment -> [Integer]
--randAgentRunTimes gen numReps numDrones timeLimit env = 