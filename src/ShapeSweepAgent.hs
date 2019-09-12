module ShapeSweepAgent where

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


data KMeansLowPolicy = KMeansLowPolicy (Map.Map DroneTerritory Footprint)
  deriving (Eq, Show)

--only call this after checking that there are moves to apply for each idle drone
applyMoves :: EnsembleStatus -> KMeansLowPolicy -> (NextActions, KMeansLowPolicy)
applyMoves enStat (KMeansLowPolicy map) = (catMaybes maybeAssignments, newPolicy)
  where
    (maybeAssignments, newPolicy) = Map.foldrWithKey (accumulateNextMoves enStat) ([], KMeansLowPolicy $ Map.empty) map

accumulateNextMoves :: EnsembleStatus -> DroneTerritory -> Footprint -> ([Maybe (Drone, Action)], KMeansLowPolicy) -> ([Maybe (Drone, Action)], KMeansLowPolicy)
accumulateNextMoves enStat dt fp (na, KMeansLowPolicy map) = (newAction : na, KMeansLowPolicy $ Map.insert newKey fp map)
  where
    (newAction, newKey) = applyMove enStat dt

applyMove :: EnsembleStatus -> DroneTerritory -> (Maybe (Drone, Action), DroneTerritory)
applyMove enStat dt@(DroneTerritory drone mean dirs) =
  if (idleOrUnlisted enStat dt)
    then (Just $ (drone, head dirs), DroneTerritory drone mean $ tail dirs)
    else (Nothing, dt)

--uses A* and the current territory assignments to assign what the idle and unassigned drones should do next
--should prioritize visiting the territory farthest from any other means
assignDirections :: WorldView -> KMeansLowPolicy -> KMeansLowPolicy
assignDirections wv p@(KMeansLowPolicy map) = KMeansLowPolicy $ Map.foldrWithKey (setDirections wv) Map.empty map
  where
    droneTerritories = Map.keysSet map

setDirections :: WorldView -> DroneTerritory -> Footprint -> Map.Map DroneTerritory Footprint -> Map.Map DroneTerritory Footprint
setDirections wv@(WorldView envInfo enStat) dt@(DroneTerritory drone mean dirs) fp soFar = Map.insert newKey fp soFar
  where
    newKey =
      case dirs of
        (dir : rest) -> dt
        [] -> DroneTerritory drone mean newDirs

    newDirs = fromMaybe [Hover] maybeDirections

    maybeDirections = maybePath >>= makeDirections
    maybePath = currentPos >>= (\cp -> aStar envInfo mkManhattanHeuristic cp targetPos)

    --eventually refactor to provide list of all means to this function, then make this step pick the position farthest from all foreign means
    targetPos :: Position
    targetPos = if (null toVisit)
                  then minPos
                  else Set.findMin toVisit

    toVisit = Set.intersection fp $ Map.keysSet $ Map.filter (not . isFullyObserved) envInfo

    currentPos :: Maybe Position
    currentPos = fmap groundPos $ lookup drone enStat

    minPos = fst $ Map.findMin envInfo

initializeKMP :: Int -> StdGen -> WorldView -> KMeansLowPolicy
initializeKMP iterations gen wv@(WorldView envInfo enStat) = KMeansLowPolicy $ kMeans iterations gen fp dSeq
  where
    fp = Map.keysSet envInfo

    dSeq = SQ.fromList $ fmap (\(drone, pos) -> DroneTerritory drone pos []) dronesList

    dronesList :: [(Drone, Position)]
    dronesList = fmap (fmap groundPos) enStat


instance Policy KMeansLowPolicy where
  nextMove p@(KMeansLowPolicy map) wv@(WorldView envInfo enStat) = 
    if (anyWaiting enStat $ Map.keysSet map)
      then applyMoves enStat $ assignDirections wv $ KMeansLowPolicy $ kMeansInternal 1 map --need to assign new moves after the K-means step
      else applyMoves enStat p--just need to assign any idle drones to the next task in its directions list in this case

   


    --if one of the drones needs 

--a second, "HighSweepPolicy" should probably use the same policy as LowSweep for its "phase 2" behavior, after the high environment has been explored and it has descended again
--highsweepPolicy should use a smart function to query A* for paths to a filtered subset of nodes that make sense to visit (e.g rows 2, 5, 8, etc. with shape caveats)

