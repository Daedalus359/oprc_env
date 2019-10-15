module ShapeSweepAgent where

import AgentUtils
import GraphOPRC
import Policy
import Drone
import EnvView
import Ensemble
import Env
import WorldState

import Data.Maybe
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import System.Random
import qualified Data.Sequence as SQ


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
                    then (zip (fmap DroneID [1 .. ]) (take numDrones $ repeat fixAlt), LowSweepPolicy [])
                    else (zip (fmap DroneID [1 .. ]) (take numDrones $ repeat $ head directions), LowSweepPolicy $ tail directions)

                fixAlt = case droneAlt of
                           High -> MoveVertical Descend
                           Low -> Hover

                numDrones = numDronesRunning wv

                directions = fromMaybe [Hover] maybeDirections --eventually do something more interesting?

                maybeDirections :: Maybe Directions
                maybeDirections = maybePath >>= makeDirections
                maybePath = aStarStandardPenalty Low envInfo mkManhattanHeuristic startPos nextPosToVisit

                startPos :: Position
                startPos = getEnvPos dronePos
                nextPosToVisit = if (null unobservedMap)
                                   then minPos
                                   else fst $ Map.findMin unobservedMap

                droneStat = snd $ head enStat
                dronePos = posFromStat droneStat
                droneAlt = getEnvAlt dronePos

                unobservedMap = Map.filter (not . isFullyObserved) envInfo
                minPos = fst $ Map.findMin envInfo

      --don't command any actions if at least one drone is still in the middle of acting
      else ([], p)


data HighSweepPolicy = HighSweepPolicy
  {
    getPhase :: AltitudePhase
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

data KMeansLowPolicy = KMeansLowPolicy StdGen (Map.Map DroneTerritory Footprint)
  deriving Show

initializeKMP :: Int -> StdGen -> WorldView -> KMeansLowPolicy
initializeKMP iterations gen wv@(WorldView envInfo enStat) = KMeansLowPolicy gen2 $ kMeansLow iterations gen1 envInfo dSeq
  where
    dSeq = SQ.fromList $ fmap (\(drone, pos) -> DroneTerritory drone pos []) dronesList

    dronesList :: [(Drone, Position)]
    dronesList = fmap (fmap groundPos) enStat

    (gen1, gen2) = split gen

instance Policy KMeansLowPolicy where
  nextMove p@(KMeansLowPolicy gen map) wv@(WorldView envInfo enStat) =

    applyMoves enStat gen2 directedMap

    where
      directedMap = assignDirections setDirections wv reassignedMap --make sure all drones are either acting or have a list of actions to get a new assignment from
      reassignedMap = kMeansInternal incompleteLocations gen1 envInfo 1 map --reassign territory to each drone
      (gen1, gen2) = split gen

instance DroneTerritoryMapPolicy KMeansLowPolicy where
  getMap (KMeansLowPolicy gen map) = map
  fromMap gen map = KMeansLowPolicy gen map

--once this has been applied to all DroneTerritories, every drone will either have a list of directions to follow or will be busy completing a motion
setDirections :: WorldView -> Set.Set DroneTerritory -> DroneTerritory -> Footprint -> DroneTerritory
setDirections wv@(WorldView envInfo enStat) meansSet dt@(DroneTerritory drone mean dirs) fp =
  if (droneIsIdle && outOfDirections)--need new directions only when the drone is idle and there is not a precomputed list of what to do next
    then DroneTerritory drone mean (fixAltLow droneAlt newDirs) --evaluating newDirs causes A* to run
    else dt

  where
    outOfDirections = --in this case, there are no more pre-computed actions to assign
      case dirs of
        (action : actions) -> False
        [] -> True

    droneStat = fromJust $ lookup drone enStat --the lookup operation should never fail to find the drone's real status
    droneIsIdle = isUnassigned droneStat --will this drone need a new action assignment during this nextMove step?

    --a couple of layers of backup in case targetPos doesn't find a good choice
    backupPos :: Position 
    backupPos = fromMaybe minPos $ Set.lookupMin fp --arbitrarily chosing the minimum position in the drone's assigned territory
      where minPos = Set.findMin $ Map.keysSet envInfo --the overall environment needs to have patches in it, hence findMin

    --if A*  gets called, this is the location it will find directions to
    targetPos =
      if (null otherMeans)
        then backupPos
        else snd $ Set.findMax $ Set.map (\p -> (leastDistMeans otherMeans p, p)) fp--as far as possible from all foreign means
      
    dronePos :: DronePosition
    dronePos = posFromStat droneStat
    droneGroundPos = getEnvPos dronePos
    droneAlt = getEnvAlt dronePos --need altitude info to let A* decide the observational value of slightly longer paths



    --A* and conversion to the datatype we need
    newDirs = case maybeDirections of
                Just al@(action : actions) -> al
                _ -> [Hover] --covers both failed A* (Nothing) and case where start and end position are the same (Just [])
    maybeDirections = maybePath >>= makeDirections
    maybePath = aStarStandardPenalty droneAlt envInfo mkManhattanHeuristic droneGroundPos targetPos

    otherMeans = Set.delete dt meansSet


--OLD
--only call this after checking that there are moves to apply for each idle drone
applyMoves2 :: EnsembleStatus -> StdGen -> Map.Map DroneTerritory Footprint -> (NextActions, KMeansLowPolicy)
applyMoves2 enStat gen map = (catMaybes maybeAssignments, newPolicy)
  where
    (maybeAssignments, newPolicy) = Map.foldrWithKey (accumulateNextMoves2 enStat) ([], KMeansLowPolicy gen $ Map.empty) map

accumulateNextMoves2 :: EnsembleStatus -> DroneTerritory -> Footprint -> ([Maybe (Drone, Action)], KMeansLowPolicy) -> ([Maybe (Drone, Action)], KMeansLowPolicy)
accumulateNextMoves2 enStat dt fp (na, KMeansLowPolicy gen map) = (newAction : na, KMeansLowPolicy gen $ Map.insert newKey fp map)
  where
    (newAction, newKey) = applyMove2 enStat dt

applyMove2 :: EnsembleStatus -> DroneTerritory -> (Maybe (Drone, Action), DroneTerritory)
applyMove2 enStat dt@(DroneTerritory drone mean dirs) =
  if (idleOrUnlisted enStat dt)
    then case dirs of --this should not be necessary!
           (d : ds) -> (Just $ (drone, head dirs), DroneTerritory drone mean $ tail dirs)
           [] -> (Just (drone, Hover), DroneTerritory drone mean [])--get rid of this!
    else (Nothing, dt)

--hard coding 10 may be a bad idea
instance PersistentPolicy KMeansLowPolicy where
  cleanup p@(KMeansLowPolicy gen map) = initializeKMP 10 gen