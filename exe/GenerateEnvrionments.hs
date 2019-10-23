module Main where

import Env
import EnvGen
import qualified RandomOPRC as RO
import SerializeOPRC

import Data.Time.Clock
import System.Directory
import System.IO
import System.Random

filePrefix :: String
filePrefix = "./test/environments/generated/"

numEnvs = 9

main :: IO ()
main = foldr writeFilesAccum makeDateDir [1 .. numEnvs]

  where
    makeDateDir :: IO ()
    makeDateDir = do
      dtDir <- fmap ((++) filePrefix) getCurrentTimeStr
      createDirectory dtDir
      setCurrentDirectory dtDir

    getCurrentTimeStr :: IO String
    getCurrentTimeStr = show <$> getCurrentTime

    writeFilesAccum :: Int -> IO () -> IO () 
    writeFilesAccum i soFar = soFar >> (envString >>= (writeCommandIndexed i))

    writeCommandIndexed :: Int -> String -> IO ()
    writeCommandIndexed i = writeFile $ "./" ++ "testMixed" ++ (show i) ++ ".env"

    envString :: IO String
    envString = encodeEnv <$> ioEnv

    ioEnv :: IO Environment
    ioEnv = (\gen -> sampleEnvironment gen eg) <$> newStdGen

    eg :: EnvGen
    eg = MixedGen [(33.0, fstGen), (67.0, sndGen)]

    fstGen :: EnvGen
    fstGen = mkEGBernoulli 0.1 6 0 90 0 45

    sndGen :: EnvGen
    sndGen = mkEGBernoulli 0.9 6 0 45 0 90

writeCustomEnv :: String -> EnvGen -> IO ()
writeCustomEnv suffix envGen = envString >>= (writeCommand suffix)
  where
    envString = encodeEnv <$> finalEnv

    finalEnv :: IO Environment
    finalEnv = fmap (\stdgen -> sampleEnvironment stdgen envGen) newStdGen

    writeCommand :: String -> String -> IO ()
    writeCommand suffix = writeFile $ filePrefix ++ suffix

    filePrefix :: String
    filePrefix = "./test/environments/generated/"

writeClumpedEnv :: Float -> Float -> IO ()
writeClumpedEnv neighborEffect initThreshold = writeCustomEnv ("clumpedNE=" ++ (show neighborEffect) ++ "T=" ++ (show initThreshold) ++ ".env") eg
  where
    eg = islandsEnvGen varLimit xMin xMax yMin yMax posAdder
    varLimit = 6
    xMin = 0
    xMax = 50
    yMin = 0
    yMax = 90
    posAdder = likeNeighbors neighborEffect initThreshold


writeBernoulliEnv :: Double -> IO ()
writeBernoulliEnv threshold = writeCustomEnv ("bernoulli" ++ (show threshold) ++ ".env") eg
  where
    eg = mkEGBernoulli threshold varLimit xMin xMax yMin yMax
    varLimit = 6
    xMin = 0
    xMax = 25
    yMin = 0
    yMax = 45

--original one, has a hole
writeBernoulliEnv2 :: IO ()
writeBernoulliEnv2 = envString >>= writeCommand

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
