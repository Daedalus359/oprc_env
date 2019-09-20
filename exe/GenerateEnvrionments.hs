module Main where

import Env
import EnvGen
import qualified RandomOPRC as RO
import SerializeOPRC

import System.IO
import System.Random

main :: IO ()
main = envString >>= writeCommand

  where
    writeCommand :: String -> IO ()
    writeCommand = writeFile $ filePrefix ++ "withHole.env"

    filePrefix :: String
    filePrefix = "./test/environments/generated/"

    envString :: IO String
    envString = encodeEnv <$> finalEnv

    finalEnv :: IO Environment
    finalEnv = holeTransform <*> bernoulliEnv

    bernoulliEnv :: IO Environment
    bernoulliEnv = bernoulliF <$> newStdGen
    bernoulliF :: StdGen -> Environment
    bernoulliF = (\gen -> RO.newBernoulliEnv gen varLimit xMin xMax yMin yMax threshold)
    varLimit = 6
    xMin = 0
    xMax = 25
    yMin = 0
    yMax = 45
    threshold = 0.27

    holeTransform :: IO (Environment -> Environment)
    holeTransform = holeF <$> newStdGen
    holeF :: StdGen -> Environment -> Environment
    holeF = (\gen -> \env -> hole gen env ctr radius)
    ctr = Position 12 20
    radius = 5
