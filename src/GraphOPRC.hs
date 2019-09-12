module GraphOPRC where

import qualified Data.Map as MapL
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.PSQueue as Q
import Data.List
import Data.Maybe
import System.Random as Random
import qualified Data.Sequence as SQ
import Data.Foldable
import Data.Monoid
import Data.Semigroup

import MoveCosts
import Env
import EnvView
import Drone
import Ensemble

--allows for efficiently building a Path as a list from back to front
data PathStep = PathStep Position (Maybe ParentPos)

type ParentPos = Position
type Path = [Position]

type EstTotalCost = MapL.Map Position Int

initializeETC :: Footprint -> EstTotalCost
--number of patches in footprint is a good upper bound for the initial f values
initializeETC fp = MapL.fromSet (const numPatches) fp
  where numPatches = Set.size fp

--CostFromStart x represents the cost of the cheapest *known* path from start to x
type CostFromStart = Map.Map Position Int

--not exactly the same as initializeETC because of strictness differences
initializeCFS :: Footprint -> CostFromStart
initializeCFS fp = Map.fromSet (const numPatches) fp
  where numPatches = Set.size fp

type Heuristic = Position -> Int

type ParentMap = Map.Map Position Position
-- empty :: ParentMap

recreatePath :: Position -> Position -> ParentMap -> Maybe Path
recreatePath start end pMap = recreatePathInternal start end pMap []

recreatePathInternal :: Position -> Position -> ParentMap -> Path -> Maybe Path
recreatePathInternal start end pMap [] = recreatePathInternal start end pMap [end]
recreatePathInternal start end pMap partialPath@(front : positions) = 
  case Map.lookup front pMap of
    (Just parent) -> recreatePathInternal start end pMap (parent : partialPath)
    Nothing -> if (front == start) then (Just partialPath) else Nothing

aStar :: EnvironmentInfo -> (Position -> Heuristic) -> Position -> Position -> Maybe Path
aStar envInfo hFunc startPos endPos = recreatePath startPos endPos $ aStarInternal fp (hFunc endPos) startPos endPos openSet Set.empty fMax cMax (Map.empty :: ParentMap)
 where
   openSet = (Q.singleton startPos 0 :: Q.PSQ Position Int)
   fMax = initializeETC fp
   cMax = initializeCFS fp
   fp = toFootprint envInfo

--continue making recursive calls until endPos can be entered into the ParentMap
aStarInternal :: Footprint -> Heuristic -> Position -> Position -> Q.PSQ Position Int -> Set.Set Position -> EstTotalCost -> CostFromStart -> ParentMap -> ParentMap
aStarInternal fp h startPos endPos openSet closedSet f c parentMap =
  case mostPromising of
    Nothing -> parentMap --if the open set is empty, A* should have either found an answer or failed (more likely)
    (Just position) ->
      if (position == endPos)
        then parentMap --A* has succeeded
        else aStarInternal fp h startPos endPos newOpenSet newClosedSet newF newC newParentMap
          where
            --updates related to the improvedNeighbors
            newF = foldr (\neighbor -> Map.insert neighbor $ h neighbor + posCost + 1) fWithNewNeighbors improvedNeighbors
            newC = foldr (\neighbor -> Map.insert neighbor (posCost + 1)) cWithNewNeighbors improvedNeighbors
            newParentMap = foldr (\child -> Map.insert child position) parentMapWithNewNeighbors improvedNeighbors
            newOpenSet = updateFromLists improvedNeighbors improvedNeighborCosts openSetWithNewNeighbors
            improvedNeighborCosts = fmap (costFrom h posCost) improvedNeighbors --new f values

            --those oldNeighbors for which a lower cost from start has just been found
            improvedNeighbors = filter (\n -> (<) (posCost + 1) $ fromMaybe fpSize $ Map.lookup n c) oldNeighbors

            --updates related to neighbors that haven't been seen at all so far
            fWithNewNeighbors = foldr (\neighbor -> Map.insert neighbor $ h neighbor + posCost + 1) f newNeighbors
            cWithNewNeighbors = foldr (\neighbor -> Map.insert neighbor (posCost + 1)) c newNeighbors
            parentMapWithNewNeighbors = foldr (\child -> Map.insert child position) parentMap newNeighbors
            openSetWithNewNeighbors = insertFromLists newNeighbors newNeighborCosts openSetNoCurrent --redo this as a fold
            newNeighborCosts = fmap (costFrom h posCost) newNeighbors

            --get the neighbors that have not already been visited
            (oldNeighbors, newNeighbors) = partition (memberOfPSQ openSetNoCurrent) neighborsToExplore
            neighborsToExplore = filter (\a -> not $ Set.member a newClosedSet) $ inBoundsNeighborsOf fp position

            --move current position from open set to closed set
            openSetNoCurrent = Q.delete position openSet
            newClosedSet = Set.insert position closedSet

            --useful numbers
            fpSize = Set.size fp
            posCost = fromMaybe fpSize $ Map.lookup position c

  where
    mostPromising = fmap Q.key $ Q.findMin openSet --this has type Maybe Position

--returns f value (c + h)
costFrom :: Heuristic -> Int -> Position -> Int
costFrom h parentCost child = h child + costFromStart
  where costFromStart = parentCost + 1 :: Int

memberOfPSQ :: (Ord k, Ord p) => Q.PSQ k p -> k -> Bool
memberOfPSQ psq k =
  case (Q.lookup k psq) of
    (Just _) -> True
    Nothing -> False

insertFromLists :: (Ord k, Ord p) => [k] -> [p] -> Q.PSQ k p -> Q.PSQ k p
insertFromLists [] _ psq = psq
insertFromLists _ [] psq = psq
insertFromLists (k : ks) (p : ps) psq = insertFromLists ks ps (Q.insert k p psq)

updateFromLists :: (Ord k, Ord p) => [k] -> [p] -> Q.PSQ k p -> Q.PSQ k p
updateFromLists [] _ psq = psq
updateFromLists _ [] psq = psq
updateFromLists (k : ks) (p : ps) psq = updateFromLists ks ps (Q.adjust (const p) k psq)

insertFromList :: (Ord k, Ord p) => (k -> p) -> [k] -> Q.PSQ k p -> Q.PSQ k p
insertFromList f [] psq = psq
insertFromList f (k : kList) psq = insertFromList f kList (Q.insert k (f k) psq)

--useful heuristic functions go here
mkManhattanHeuristic :: Position -> Heuristic
mkManhattanHeuristic endPos = manhattanDistance endPos

manhattanDistance :: Position -> Position -> Int
manhattanDistance pos1@(Position x1 y1) pos2@(Position x2 y2) = deltaX + deltaY
  where
    deltaX = abs $ x1 - x2
    deltaY = abs $ y1 - y2

--may not make sense to keep the toList stuff around - decide what form I need this in
kMeans :: HasCenter d => Int -> StdGen -> Footprint -> SQ.Seq d -> Map.Map d Footprint
kMeans iterations gen footprint droneSeq = kMeansInternal iterations initMap
  where
    --initMap :: HasCenter d => Map.Map d Footprint
    initMap = Map.fromList $ toList $ SQ.zip keys kSplits

    keys = SQ.zipWith moveCenter kMeans droneSeq

    kMeans = fmap avgPos kSplits
    kSplits = fst $ foldr assignAtRandom (SQ.replicate k Set.empty, gen) footprint

    k = SQ.length droneSeq

--don't call with a number of iterations less than zero!
--possible bug - avgPos will always move to (0, 0) if there are no patches in a footprint. Is this desirable behavior?
kMeansInternal :: HasCenter d => Int -> Map.Map d Footprint -> Map.Map d Footprint
kMeansInternal 0 map = map
kMeansInternal iterations map = kMeansInternal (iterations - 1) newMap
  where
    newMap = Set.foldr (\key -> \soFar -> Map.unionWith Set.union soFar $ Map.singleton key $ Set.empty) correctedMeans means

    --inefficient! Probably worth reimplementing this using toList or something
    --now that reassignments have been made, correct the mean value assigned to each footprint.
    correctedMeans = Map.foldrWithKey (\needsMeanUpdate -> \fp -> \soFar -> Map.union soFar $ Map.singleton (moveCenter (avgPos fp) needsMeanUpdate) fp) Map.empty reassignedMap  

    reassignedMap = Map.foldrWithKey (\oldMean -> \fp -> \soFar -> Map.unionWith Set.union soFar $ reassign oldMean fp) Map.empty map


    --assignmentList = Map.foldrWithKey (\oldMean -> \oldFp -> \soFar -> Set.union soFar $ makeTuples oldMean oldFp) Set.empty map

    --reassign :: HasCenter d => d -> Footprint -> Map.Map d Footprint
    reassign mean fp = foldr (\(aMean, aPos) -> \b -> Map.unionWith Set.union b $ Map.singleton aMean $ Set.singleton aPos) Map.empty newAssignments
      where
        newAssignments = fmap (\(oldMean, pos) -> (nearestMean means oldMean pos, pos)) oldAssignments
        oldAssignments = makeTuples mean fp

    --makeTuples :: HasCenter d => d -> Footprint -> [(d, Position)]
    makeTuples mean fp = fmap (\pos -> (mean, pos)) $ toList fp

    means = Map.keysSet map

nearestMean :: HasCenter d => Set.Set d -> d -> Position -> d
nearestMean means currentMean pos = foldr (closerTo pos) currentMean means

--returns whichever of args 2 and 3 is closer to arg 1, with preference for arg 2 in case of a tie
closerTo :: HasCenter d => Position -> d -> d -> d
closerTo pos mean1 mean2 =
  if (dist2 < dist1) then mean2 else mean1
  where
    dist2 = idealDistance pos $ getCenter mean2
    dist1 = idealDistance pos $ getCenter mean1

--"closeness" is based on maximum utilization of diagonal motion (shortest possible path assuming no obstacles)
idealDistance :: Position -> Position -> Int
idealDistance (Position x1 y1) (Position x2 y2) = diagCost * diagonalMoves + straightCost * straightMoves
  where
    --just need the type to get an answer from cost
    straightCost = cost (undefined :: CardinalDir)
    diagCost = cost (undefined :: IntercardinalDir)

    straightMoves = abs deltaX - deltaY
    diagonalMoves = min deltaX deltaY

    deltaX = abs $ x1 - x2
    deltaY = abs $ y1 - y2

avgPos :: Footprint -> Position
avgPos ftp = Position (quot sumX sz) (quot sumY sz)
  where
    (sumX, sumY) = foldr accumulate (0, 0) ftp
    sz = max 1 $ Set.size ftp --max prevents divide by zero when the set is empty

    accumulate :: Position -> (Int, Int) -> (Int, Int)
    accumulate (Position x y) (xTot, yTot) = (xTot + x, yTot + y)

assignAtRandom :: Ord a => a -> (SQ.Seq(Set.Set a), StdGen) -> (SQ.Seq(Set.Set a), StdGen)
assignAtRandom a (sets, gen) = (SQ.adjust (Set.insert a) i sets, newGen)
  where
    (i, newGen) = randomR (0, k - 1) gen
    k = SQ.length sets

data DroneTerritory = DroneTerritory
  { getDrone :: Drone
  , getMean :: Position
  , getDirsDT :: Directions
  }
  deriving (Eq, Show)

instance Ord DroneTerritory where
  compare (DroneTerritory d1 _ _) (DroneTerritory d2 _ _) = compare d1 d2

anyWaiting :: EnsembleStatus -> Set.Set DroneTerritory -> Bool
anyWaiting enStat territories = getAny $ foldMap (idleAndUndirected enStat) territories

idleAndUndirected :: EnsembleStatus -> DroneTerritory -> Any
idleAndUndirected enStat dt@(DroneTerritory drone mean dirs) =
  case dirs of
    (action : actions) -> Any False
    [] -> if (idleOrUnlisted enStat dt)
            then Any True
            else Any False

idleOrUnlisted :: EnsembleStatus -> DroneTerritory -> Bool
idleOrUnlisted enStat (DroneTerritory drone _ _) = (fromMaybe True $ fmap isUnassigned $ lookup drone enStat)

--agents that execute paths based purely on the footprint of the environment
type Directions = [Action]

class Ord d => HasCenter d where
  getCenter :: d -> Position
  moveCenter :: Position -> d -> d

--for working directly with 
instance HasCenter Position where
  getCenter = id
  moveCenter p = const p

instance HasCenter DroneTerritory where
  getCenter = getMean
  moveCenter newMean (DroneTerritory drone oldMean dirs) = DroneTerritory drone newMean dirs