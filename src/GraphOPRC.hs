module GraphOPRC where

import qualified Data.Map as MapL
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.PSQueue as Q
import Data.List
import Data.Maybe
import System.Random as Random
import qualified Data.Sequence as SQ

import Env
import EnvView

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

kMeans :: StdGen -> Int -> Footprint -> [Footprint]
kMeans gen k footprint = kMeansInternal kMeans kSplits
  where
    kMeans = fmap undefined kSplits
    kSplits = fst $ foldr assignAtRandom (SQ.replicate k Set.empty, gen) footprint

kMeansInternal = undefined

avgPos :: Footprint -> Position
avgPos ftp = undefined
  where
    sumPos = foldr accumulate (0, 0) ftp
    sz = Set.size ftp

    accumulate :: Position -> (Int, Int) -> (Int, Int)
    accumulate (Position x y) (xTot, yTot) = (xTot + x, yTot + y)

assignAtRandom :: Ord a => a -> (SQ.Seq(Set.Set a), StdGen) -> (SQ.Seq(Set.Set a), StdGen)
assignAtRandom a (sets, gen) = (SQ.adjust (Set.insert a) i sets, newGen)
  where
    (i, newGen) = randomR (0, k - 1) gen
    k = SQ.length sets