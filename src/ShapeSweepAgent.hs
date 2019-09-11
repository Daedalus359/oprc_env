module ShapeSweepAgent where

import GraphOPRC
import Policy
import Drone
import EnvView
import Ensemble
import Env

import Data.Maybe
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set



--commands all drones to follow along the same path, obviously inefficient
data LowSweepPolicy = LowSweepPolicy
 {
   getDirections :: Directions
 }
 deriving Show

instance Policy LowSweepPolicy where
  nextMove p@(LowSweepPolicy actions) wv@(WorldView envInfo enStat) = 
    if (allIdle enStat)
      then case actions of
             --tell all drones to do the same next action
             (action : moreActions) -> (nextActions, restOfP)
               where
                nextActions = zip (fmap DroneID [1 .. ]) (take numDrones $ repeat action)
                restOfP = LowSweepPolicy moreActions
                numDrones = numDronesRunning wv

             --use A* to figure out the next path to command, then convert this to directions and start following
             [] -> (nextActions, restOfP)--nextMove (LowSweepPolicy directions) wv
               where
                --a recursive call makes more sense, but trying this way for debug purposes
                (nextActions, restOfP) =
                  if (null directions)
                    then (zip (fmap DroneID [1 .. ]) (take numDrones $ repeat Hover), LowSweepPolicy [])
                    else (zip (fmap DroneID [1 .. ]) (take numDrones $ repeat $ head directions), LowSweepPolicy $ tail directions)
                numDrones = numDronesRunning wv

                directions = fromMaybe [Hover] maybeDirections --eventually do something more interesting?

                maybeDirections :: Maybe Directions
                maybeDirections = maybePath >>= makeDirections
                maybePath = aStar envInfo mkManhattanHeuristic startPos nextPosToVisit

                startPos :: Position
                startPos = groundPos $ snd $ head enStat
                nextPosToVisit = if (null unobservedMap)
                                   then minPos
                                   else fst $ Map.findMin unobservedMap

                unobservedMap = Map.filter (not . isFullyObserved) envInfo
                minPos = fst $ Map.findMin envInfo

      --don't command any actions if at least one drone is still in the middle of acting
      else ([], p)



makeDirections :: Path -> Maybe Directions
makeDirections [] = Just []
makeDirections (pos : []) = Just []
makeDirections (pos1 : rest@(pos2 : path)) = 
  case (toAction (getHop pos1 pos2)) of
    Nothing -> Nothing
    (Just action) -> fmap ((:) action) $ makeDirections rest

data SweepPhase = HighSweep | LowSweep

data HighSweepPolicy = HighSweepPolicy
  {
    getPhase :: SweepPhase
  , getDirs :: Directions
  }

instance Policy HighSweepPolicy where
  nextMove p@(HighSweepPolicy LowSweep dirs) wv = fmap toHighSweep $ nextMove (LowSweepPolicy dirs) wv
  nextMove p@(HighSweepPolicy HighSweep dirs) wv = undefined

--just converts between data types. Since the LowSweep phases is used, it does not alter the associated behavior of the policy
toHighSweep :: LowSweepPolicy -> HighSweepPolicy
toHighSweep (LowSweepPolicy dirs) = HighSweepPolicy LowSweep dirs

highSweepPoints :: Footprint -> Footprint
highSweepPoints envFp = foldr (accumulateSweepLocations envFp) Set.empty envFp

--take nearestSweepPos and make a version that is of type Footprint -> Position -> Footprint -> Footprint for use in a fold
accumulateSweepLocations :: Footprint -> Position -> Footprint -> Footprint
accumulateSweepLocations envFp pos visitFp = Set.insert (nearestSweepPos envFp pos) visitFp

nearestSweepPos :: Footprint -> Position -> Position
nearestSweepPos fp pos@(Position x y) =
  if (Set.member perfectPos fp)
    then perfectPos
    else if (Set.member secondChoice fp)
           then secondChoice
           else if (Set.member thirdChoice fp)
            then thirdChoice
            else pos

  where
    secondChoice = neighborTo North perfectPos
    thirdChoice = neighborTo South perfectPos

    perfectPos = (Position perfectX y)
    perfectX =
      case (mod x 3) of
        0 -> x
        1 -> x - 1
        2 -> x + 1


data KMeansLowPolicy = KMeansLowPolicy [DroneTerritory]

instance Policy KMeansLowPolicy where
  nextMove p@(KMeansLowPolicy []) _ = ([], p) --base case for recursion over [DroneTerritory]
  nextMove p@(KMeansLowPolicy dts@(dt : rest)) wv@(WorldView envInfo enStat) = 
    if (anyWaiting enStat dts)
      then undefined--run an iteration of k-means to redistribute territories between drones
      else undefined--just need to assign any idle drones to the next task in its directions list in this case

    --if one of the drones needs 

--a second, "HighSweepPolicy" should probably use the same policy as LowSweep for its "phase 2" behavior, after the high environment has been explored and it has descended again
--highsweepPolicy should use a smart function to query A* for paths to a filtered subset of nodes that make sense to visit (e.g rows 2, 5, 8, etc. with shape caveats)

