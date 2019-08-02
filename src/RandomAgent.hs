module RandomAgent where

import Policy
import Drone
import Env
import EnvView
import Ensemble

--import Test.QuickCheck
import System.Random as Random
import Data.Maybe

--RandomPolicy has no state other than the current RandomGen, which 
newtype RandomPolicy = RandomPolicy StdGen

--lets you make an random blind policy out of a (random seed) Int value
mkRBP :: Int -> RandomPolicy
mkRBP = RandomPolicy . mkStdGen

randomCardinal :: StdGen -> (CardinalDir, StdGen)
randomCardinal stdGen = (dir, gen2)
  where
    (dirNum, gen2) = randomR (0, 3) stdGen :: (Int, StdGen)
    dir = case dirNum of
            0 -> North
            1 -> South
            2 -> East
            3 -> West

randomIntercardinal :: StdGen -> (IntercardinalDir, StdGen)
randomIntercardinal stdGen = (dir, gen2)
  where
    (dirNum, gen2) = randomR (0, 3) stdGen :: (Int, StdGen)
    dir = case dirNum of
            0 -> NE
            1 -> SE
            2 -> NW
            3 -> SW

randomVertical :: StdGen -> (VerticalDirection, StdGen)
randomVertical stdGen = (dir, gen2)
  where
    (dirNum, gen2) = randomR (0, 1) stdGen :: (Int, StdGen)
    dir = case dirNum of
            0 -> Ascend
            1 -> Descend

randomAction :: StdGen -> (Action, StdGen)
randomAction stdGen =
  case actionType of
    0 -> (MoveCardinal (fst $ randomCardinal gen3), gen4)
    1 -> (MoveIntercardinal (fst $ randomIntercardinal gen3), gen4)
    2 -> (MoveVertical (fst $ randomVertical gen3), gen4)
    3 -> (Hover, gen2)
  where
    (actionType, gen2) = (randomR (0, 3) stdGen :: (Int, StdGen))
    (gen3, gen4) = split gen2

randomActions :: Int -> StdGen -> (Maybe [Action], StdGen)
randomActions i stdGen
  | i < 0 = (Nothing, stdGen)
  | i == 0 = (Just [], stdGen)
  | i > 0 = addToResult action (randomActions (i - 1) nextGen)
  where
    (action, nextGen) = randomAction stdGen
    addToResult :: Action -> (Maybe [Action], StdGen) -> (Maybe [Action], StdGen)
    addToResult action (Nothing, aGen) = (Nothing, aGen)
    addToResult action (Just actionList, aGen) = (Just (action : actionList), aGen)

instance Policy RandomPolicy where
  nextMove (RandomPolicy stdGen) worldView =
    (nextActions, RandomPolicy newGen)
    where
      currentStatus = getEnsembleStatus worldView
      dronesNeedingActions = needsCommand currentStatus

      numDrones = length dronesNeedingActions
      (maybeAL, newGen) = randomActions numDrones stdGen
      allHover = take numDrones $ repeat Hover
      actionsList = fromMaybe allHover maybeAL

      nextActions = zip dronesNeedingActions actionsList