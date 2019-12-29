module RandomOPRC where

import RandomAgent
import Scenario
import Env
import EnvGen
import EnvView
import Policy

import System.Random as Random
import Control.Monad
import Data.Maybe
import qualified Data.Set as Set

randPolicy :: IO (RandomPolicy)
randPolicy = fmap RandomPolicy newStdGen

randGen :: IO StdGen
randGen = newStdGen

randAgentRunTime :: Int -> Integer -> Environment -> StdGen -> Maybe Integer
randAgentRunTime numDrones timeLimit env gen = timeScenarioRun timeLimit scenario
  where scenario = mkScenario (const $ RandomPolicy gen) numDrones env

randAgentRunTimes :: Int -> Int -> Integer -> Environment -> IO [Maybe Integer]
randAgentRunTimes numReps numDrones timeLimit env = (fmap . fmap) timedRun $ replicateM numReps newStdGen
  where timedRun = randAgentRunTime numDrones timeLimit env

averageRunTime :: [Maybe Integer] -> Maybe Integer
averageRunTime mbis = (fmap $ \x -> div x len) . (fmap $ foldr (+) 0) . sequenceA $ mbis
  where len = toInteger $ length mbis

newBernoulliEnv :: StdGen -> Int -> Int -> Int -> Int -> Int -> Double -> Environment
newBernoulliEnv gen varLimit xMin xMax yMin yMax threshold = bernoulliEnv bg definitelySet
  where
    definitelySet = fromMaybe Set.empty $ randomFootprint gen3 varLimit xMin xMax yMin yMax
    bg = BernoulliGen threshold gen2
    (gen2, gen3) = split gen

averageAgentPerformance :: (Policy p) => Int -> Integer -> (WorldView -> p) -> [Environment] -> IO (Maybe Integer) 
averageAgentPerformance numDrones timeLimit policyF environments = fmap averageRunTime $ (fmap $ fmap pullTime) $ traverse singleEnvEater environments
  where
    --singleEnvEater :: Environment -> IO (Bool, Scenario p)
    singleEnvEater env = fullRun timeLimit numDrones <$> ((pure :: a -> IO a) policyF) <*> (pure env)



--fullSteppableRun to create a (Bool, s p)

pullTime :: (Steppable s, Policy p) => (Bool, s p) -> Maybe Integer
pullTime (False, _) = Nothing
pullTime (True, stp) = Just $ getTimeS stp