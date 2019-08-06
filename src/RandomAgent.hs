module RandomAgent where

import Policy
import Drone
import Env
import EnvView
import Ensemble
import WorldState

--import Test.QuickCheck
import System.Random as Random
import Data.Maybe

--RandomPolicy has no state other than the current RandomGen, which 
newtype RandomPolicy = RandomPolicy
  { getGen :: StdGen }

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

newtype RandomFilteredPolicy = RandomFilteredPolicy 
  { getStdGen :: StdGen }

instance Policy RandomFilteredPolicy where
  nextMove (RandomFilteredPolicy stdGen) worldView = fmap RandomFilteredPolicy $ applyValidActions stdGen fp enStat
    where
      fp = (toFootprint $ getView worldView)
      enStat = (getEnsembleStatus worldView)

randomValidAction :: StdGen -> Footprint -> DronePosition -> (Action, StdGen)
randomValidAction gen fp dronePos = 
  case (validMove fp dronePos action && notHover action) of
    True -> (action, nextGen)
    False -> randomValidAction nextGen fp dronePos
  where
    (action, nextGen) = randomAction gen

randomValidWhenUnassigned :: StdGen -> Footprint -> DroneStatus -> (Maybe Action, StdGen)
randomValidWhenUnassigned gen fp (Unassigned dronePos) = fstJust $ randomValidAction gen fp dronePos
  where
    fstJust (a, b) = (Just a, b)
randomValidWhenUnassigned gen _ _ = (Nothing, gen)

randomMap :: RandomGen g => g -> (g -> a -> (b, g)) -> [a] -> ([b], g)
randomMap gen _ [] = ([], gen)
randomMap gen rFunc (a : as) = addTo bVal $ randomMap newGen rFunc as
  where
    addTo b (bs, g) = (b : bs, g)
    (bVal, newGen) = rFunc gen a

zipByMaybe :: [a] -> [Maybe b] -> [(a, b)]
zipByMaybe [] _ = []
zipByMaybe _ [] = []
zipByMaybe (a : as) (Just b : bs) = (a, b) : zipByMaybe as bs
zipByMaybe (a : as) (Nothing : bs) = zipByMaybe as bs

applyValidActions :: StdGen -> Footprint -> EnsembleStatus -> (NextActions, StdGen)
applyValidActions gen fp enStat = (liftFst $ zipByMaybe $ fmap fst enStat) . (randomMap gen (\g -> \ds -> randomValidWhenUnassigned g fp ds)) . (fmap snd) $ enStat
  --fmap snd takes ensemblestatus to [DroneStatus]
  --randomMap ... makes it a ([Maybe Action], StdGen)
  where
    liftFst fab (a, c) = (fab a, c)



--lets you make an random blind policy out of a (random seed) Int value
mkRP :: Int -> RandomPolicy
mkRP = RandomPolicy . mkStdGen

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

choice :: RandomGen g => g -> [a] -> (a, g)
choice gen list =
  (list !! randomIndex, newGen)
  where
    (randomIndex, newGen) = randomR (0, length list - 1) gen

weightedChoice :: RandomGen g => g -> [(a, Double)] -> (a, g)
weightedChoice gen weightedList = (fromMaybe defaultVal $ firstGT randomIndex accumulatedList, newGen)
  where
    accumulatedList = scanl1 weightAccum weightedList
    maxIndex = snd $ last $ accumulatedList
    defaultVal = fst $ last $ accumulatedList
    (randomIndex, newGen) = randomR (0, maxIndex) gen

weightAccum :: Num b => (a, b) -> (a, b) -> (a, b)
weightAccum (a1, b1) (a2, b2) = (a2, b1 + b2)

firstGT :: Ord b => b -> [(a, b)] -> Maybe a
firstGT _ [] = Nothing
firstGT index ((a, b) : list) = 
  case (b >= index) of
    True -> Just a
    False -> firstGT index list