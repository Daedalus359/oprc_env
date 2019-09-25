module Main where

import Env
import EnvGen
import qualified RandomOPRC as RO
import SerializeOPRC

import System.IO
import System.Random

filePrefix :: String
filePrefix = "./test/environments/generated/"

numEnvs = 9

main :: IO ()
main = foldr writeFilesAccum (return ()) [1 .. numEnvs] 
  where
    writeFilesAccum :: Int -> IO () -> IO () 
    writeFilesAccum i soFar = envString >>= (writeCommandIndexed i) >> soFar

    writeCommandIndexed :: Int -> String -> IO ()
    writeCommandIndexed i = writeFile $ filePrefix ++ "testMixed" ++ (show i) ++ ".env"

    envString :: IO String
    envString = encodeEnv <$> ioEnv

    ioEnv :: IO Environment
    ioEnv = (\gen -> sampleEnvironment gen eg) <$> newStdGen

    eg :: EnvGen
    eg = MixedGen [(33.0, fstGen), (67.0, sndGen)]

    fstGen :: EnvGen
    fstGen = mkEGBernoulli 0.1 6 0 25 0 45

    sndGen :: EnvGen
    sndGen = mkEGBernoulli 0.9 6 0 45 0 25

writeBernoulliEnv :: IO ()
writeBernoulliEnv = envString >>= writeCommand

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
