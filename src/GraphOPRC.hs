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
import Data.Tree

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

aStar :: Altitude -> EnvironmentInfo -> (Position -> Heuristic) -> Position -> Position -> Maybe Path
aStar droneAlt envInfo hFunc startPos endPos = recreatePath startPos endPos $ aStarInternal droneAlt envInfo fp (hFunc endPos) startPos endPos openSet Set.empty fMax cMax (Map.empty :: ParentMap)
 where
   openSet = (Q.singleton startPos 0 :: Q.PSQ Position Int)
   fMax = initializeETC fp
   cMax = initializeCFS fp
   fp = toFootprint envInfo

--continue making recursive calls until endPos can be entered into the ParentMap
aStarInternal :: Altitude -> EnvironmentInfo -> Footprint -> Heuristic -> Position -> Position -> Q.PSQ Position Int -> Set.Set Position -> EstTotalCost -> CostFromStart -> ParentMap -> ParentMap
aStarInternal droneAlt envInfo fp h startPos endPos openSet closedSet f c parentMap =
  case mostPromising of
    Nothing -> parentMap --if the open set is empty, A* should have either found an answer or failed (more likely)
    (Just position) ->
      if (position == endPos)
        then parentMap --A* has succeeded
        else aStarInternal droneAlt envInfo fp h startPos endPos newOpenSet newClosedSet newF newC newParentMap
          where
            --updates related to the improvedNeighbors
            newF = foldr (\neighbor -> Map.insert neighbor $ h neighbor + (totalCostThroughPos neighbor)) fWithNewNeighbors improvedNeighbors
            newC = foldr (\neighbor -> Map.insert neighbor (totalCostThroughPos neighbor)) cWithNewNeighbors improvedNeighbors
            newParentMap = foldr (\child -> Map.insert child position) parentMapWithNewNeighbors improvedNeighbors
            newOpenSet = updateFromLists improvedNeighbors improvedNeighborCosts openSetWithNewNeighbors
            improvedNeighborCosts = fmap (costFrom h posCost) improvedNeighbors --new f values

            --those oldNeighbors for which a lower cost from start has just been found
            improvedNeighbors = filter (\n -> (<) (totalCostThroughPos n) $ fromMaybe fpSize $ Map.lookup n c) oldNeighbors

            --updates related to neighbors that haven't been seen at all so far
            fWithNewNeighbors = foldr (\neighbor -> Map.insert neighbor $ h neighbor + (totalCostThroughPos neighbor)) f newNeighbors
            cWithNewNeighbors = foldr (\neighbor -> Map.insert neighbor (totalCostThroughPos neighbor)) c newNeighbors

            totalCostThroughPos :: Position -> Int
            totalCostThroughPos neighbor = posCost + (fromMaybe fpSize $ Map.lookup neighbor penalizedNeighborCostsFromPos)

            parentMapWithNewNeighbors = foldr (\child -> Map.insert child position) parentMap newNeighbors

            openSetWithNewNeighbors = insertFromLists newNeighbors newNeighborCosts openSetNoCurrent --redo this as a fold
            newNeighborCosts = fmap (costFrom h posCost) newNeighbors

            (oldNeighbors, newNeighbors) = partition (memberOfPSQ openSetNoCurrent) neighborsToExplore
            neighborsToExplore = Set.toList $  Map.keysSet neighborCostsFromPos

            --alter the cost function to reward moving through territory that has not yet been explored
            penalizedNeighborCostsFromPos :: Map.Map Position Int
            penalizedNeighborCostsFromPos = Map.foldrWithKey penalizeAccum neighborCostsFromPos neighborCostsFromPos

            --fix to include more context about what's going on
            penalizeAccum :: Position -> Int -> Map.Map Position Int -> Map.Map Position Int
            penalizeAccum pos geomCost map = Map.adjust (const $ penalty pi + geomCost) pos map
              where
                pi = fromMaybe Unseen $ Map.lookup pos envInfo

            penalty :: PatchInfo -> Int
            penalty Unseen = 0
            penalty (Classified _) =
              case droneAlt of
                High -> pointlessPenalty
                Low -> 0
            penalty (FullyObserved _) = pointlessPenalty

            --tune this!
            pointlessPenalty = 9

            neighborCostsFromPos :: Map.Map Position Int
            neighborCostsFromPos = Map.fromList $ filter (\(p, c) -> not $ Set.member p newClosedSet) $ MoveCosts.inBoundsNeighborsOfWithCosts fp position

            --move current position from open set to closed set
            openSetNoCurrent = Q.delete position openSet
            newClosedSet = Set.insert position closedSet

            --useful numbers
            fpSize = Set.size fp
            posCost = fromMaybe fpSize $ Map.lookup position c

            straightCost = cost (undefined :: CardinalDir)
            diagCost = cost (undefined :: IntercardinalDir)

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
manhattanDistance pos1@(Position x1 y1) pos2@(Position x2 y2) = (*) straightCost $ deltaX + deltaY
  where
    deltaX = abs $ x1 - x2
    deltaY = abs $ y1 - y2
    straightCost = cost (undefined :: CardinalDir)

--may not make sense to keep the toList stuff around - decide what form I need this in
kMeans :: Int -> StdGen -> EnvironmentInfo -> SQ.Seq DroneTerritory -> Map.Map DroneTerritory Footprint
kMeans iterations gen envInfo droneSeq = kMeansInternal nextGen envInfo iterations initMap
  where
    --initMap :: HasCenter d => Map.Map d Footprint
    initMap = Map.fromList $ toList $ SQ.zip keys kSplits

    keys = SQ.zipWith moveCenter kMeans droneSeq

    kMeans = fmap avgPos kSplits
    kSplits = fst $ foldr assignAtRandom (SQ.replicate k Set.empty, currentGen) $ incompleteLocations envInfo

    k = SQ.length droneSeq

    (nextGen, currentGen) = split gen

assignAtRandom :: Ord a => a -> (SQ.Seq(Set.Set a), StdGen) -> (SQ.Seq(Set.Set a), StdGen)
assignAtRandom a (sets, gen) = (SQ.adjust (Set.insert a) i sets, newGen)
  where
    (i, newGen) = randomR (0, k - 1) gen
    k = SQ.length sets

--don't call with a number of iterations less than zero!
kMeansInternal :: StdGen -> EnvironmentInfo -> Int -> Map.Map DroneTerritory Footprint -> Map.Map DroneTerritory Footprint
kMeansInternal _ _ 0 map = map
kMeansInternal gen envInfo iterations map = kMeansInternal nextGen envInfo (iterations - 1) newMap
  where
    --if any means have lost all of their territory, give them a random patch
    newMap = foldr (\(mean, backupTerritory) -> \existing -> Map.insert mean backupTerritory existing) correctedMeans backupAssignments
      where
        backupAssignments = zipWith littleTup newTerritories $ Set.toList leftOutMeans --a random location assigned to each of the original means that did not turn into something with territory
        littleTup pos mean = (moveCenter pos mean, Set.singleton pos)

    --now that reassignments have been made, correct the mean value assigned to each footprint.
    correctedMeans = Map.fromAscList $ fmap recenter $ Map.toAscList reassignedMap
    --can use to/from AscList because the ord instance for DroneTerritory is based on the ord instance for Drone, so fmap preserves the Ascending property
      where
        recenter (oldMean, fp) = (moveCenter (avgPos fp) oldMean, fp)

    leftOutMeans = Set.difference means preservedMeans
    preservedMeans = Map.keysSet reassignedMap

    --take all places needing observation and assign to one of the existing means (some means may have no entry in this map)
    reassignedMap = Set.foldr reassignAndAccum Map.empty placesNeedingObservation
      where
        reassignAndAccum pos soFar = Map.insertWith Set.union (newMean pos) (Set.singleton pos) soFar
        newMean pos = nearestMean means pos

    --utility values
    means = Map.keysSet map --the original keys - all of these should end up with an entry in the final map
    newTerritories = randomElems currentGen placesNeedingObservation --random unexplored locations to assign to means as needed
    (nextGen, currentGen) = split gen
    placesNeedingObservation = EnvView.incompleteLocations envInfo --the set of locations that have not been explored fully so far

randomElems :: StdGen -> Set.Set a -> [a]
randomElems gen set = fmap (\i -> setList !! i) indices
  where
    setList = Set.toList set
    indices = randomRs (0, sz - 1) gen
    sz = Set.size set

--don't call this with an empty set of means!
nearestMean :: HasCenter d => Set.Set d -> Position -> d
nearestMean means pos = foldr (closerTo pos) currentMean means
  where
    currentMean = Set.findMin means

--returns whichever of args 2 and 3 is closer to arg 1, with preference for arg 2 in case of a tie
closerTo :: HasCenter d => Position -> d -> d -> d
closerTo pos mean1 mean2 =
  if (dist2 <= dist1) then mean2 else mean1
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

    straightMoves = abs $ deltaX - deltaY
    diagonalMoves = min deltaX deltaY

    deltaX = abs $ x1 - x2
    deltaY = abs $ y1 - y2

avgPos :: Footprint -> Position
avgPos ftp = Position (f sumX sz) (f sumY sz)
  where
    f num denom = q + round
      where
        round = if (quot denom 2 >= r) then 0 else 1
        (q, r) = quotRem num denom

    (sumX, sumY) = foldr accumulate (0, 0) ftp
    sz = max 1 $ Set.size ftp --max prevents divide by zero when the set is empty

    accumulate :: Position -> (Int, Int) -> (Int, Int)
    accumulate (Position x y) (xTot, yTot) = (xTot + x, yTot + y)

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
  moveCenter newMean (DroneTerritory drone _ dirs) = DroneTerritory drone newMean dirs

--not all of these locations exist in the fine graph, but something in their "chunk" will
coarseMap :: Int -> Footprint -> Set.Set Position 
coarseMap squareDim fp = foldr checkAndAdd Set.empty fp
  where
    checkAndAdd :: Position -> Set.Set Position -> Set.Set Position
    checkAndAdd pos set =
      if (Set.member (alignedPos pos) set)
        then set
        else Set.insert (alignedPos pos) set
    alignedPos pos@(Position xc yc) = Position (align xc) (align yc)
    align x = x - (mod x squareDim)

--all of the neighbors in a coarse version of the graph
coarseNeighbors :: Int -> Position -> [Position]
coarseNeighbors squareDim (Position xc yc) = tail $ fmap Position (fmap (+ xc) changes) <*> (fmap (+ yc) changes)
  where
    changes = [0, (-squareDim), (squareDim)]

--don't run this on an empty set
dfsSpanningTree :: Int -> Set.Set Position -> Tree Position
dfsSpanningTree squareDim set = dfsInternal root visitList neighborF discoveredSet
  where
    visitList = []
    neighborF = coarseNeighbors squareDim
    discoveredSet = Set.empty
    root = Set.findMin set

dfsInternal :: Position -> [Position] -> (Position -> [Position]) -> Set.Set Position -> Tree Position
dfsInternal root visitList neighborF discoveredSet = Node root subtree
  where
    subtree = undefined
