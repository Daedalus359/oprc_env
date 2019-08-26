module SerializeOPRC where

import Env

import qualified Data.Set as Set
import qualified Data.Map.Strict as Map
import Data.Maybe

encodeEnv :: Environment -> String
encodeEnv env = go yRange xRange ""
  where
    go relativeX relativeY partialStr
      | (relativeX < 0) = go xRange (relativeY - 1) partialStr
      | (relativeY < 0) = partialStr
      | (relativeX /= xRange) = go (relativeX - 1) relativeY $ code relativeX relativeY : partialStr
      | (relativeX == xRange) = go (relativeX - 1) relativeY $ code relativeX relativeY : ( '\n' : partialStr)

    code relativeX relativeY = getCharCode env (xMin + relativeX) (yMin + relativeY)

    yRange = yMax - yMin
    xRange = xMax - xMin

    (xMin, xMax, yMin, yMax) = findLimits env


getCharCode :: Environment -> Int -> Int -> Char
getCharCode (Environment envMap) x y = 
  case (Map.lookup (Position x y) envMap) of
    Nothing -> ' ' --empty location
    Just (Patch Close) -> 'H' --"high scrutiny"
    Just (Patch Far) -> 'L' --"low scrutiny"

--xMin, xMax, yMin, and yMax for an environment
findLimits :: Environment -> (Int, Int, Int, Int)
findLimits env@(Environment eMap) = (xMin, xMax, yMin, yMax)
  where

    yMax = foldr yMaxF sampleY fp
    yMin = foldr yMinF sampleY fp
    xMax = foldr xMaxF sampleX fp
    xMin = foldr xMinF sampleX fp

    sampleX = findXc samplePos
    sampleY = findYc samplePos

    samplePos = fromMaybe (Position 0 0) $ fmap fst $ Set.minView fp
    fp = Map.keysSet eMap

xMinF :: Position -> Int -> Int
xMinF (Position x _) i = min x i

xMaxF :: Position -> Int -> Int
xMaxF (Position x _) i = max x i

yMinF :: Position -> Int -> Int
yMinF (Position _ y) i = min y i

yMaxF :: Position -> Int -> Int
yMaxF (Position _ y) i = max y i

--test/environments/generated