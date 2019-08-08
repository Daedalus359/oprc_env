module GraphOPRC where

import qualified Data.Map as MapL
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.PSQueue as Q

import Env
import EnvView

--allows for efficiently building a Path as a list from back to front
data PathStep = PathStep Position (Maybe ParentPos)

type ParentPos = Position
type Path = [Position]

type EstTotalCost = MapL.Map Position Integer

initializeETC :: Footprint -> EstTotalCost
--number of patches in footprint is a good upper bound for the initial f values
initializeETC fp = MapL.fromSet (const numPatches) fp
  where numPatches = toInteger $ Set.size fp

--CostFromStart x represents the cost of the cheapest *known* path from start to x
type CostFromStart = Map.Map Position Integer

--not exactly the same as initializeETC because of strictness differences
initializeCFS :: Footprint -> CostFromStart
initializeCFS fp = Map.fromSet (const numPatches) fp
  where numPatches = toInteger $ Set.size fp

type Heuristic = Position -> Integer

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
aStar envInfo hFunc startPos endPos = recreatePath startPos endPos $ aStarInternal fp (hFunc endPos) startPos endPos openSet fMax cMax (Map.empty :: ParentMap)
 where
   openSet = (Q.singleton startPos 0 :: Q.PSQ Position Integer)
   fMax = initializeETC fp
   cMax = initializeCFS fp
   fp = toFootprint envInfo

--continue making recursive calls until endPos can be entered into the ParentMap
aStarInternal :: Footprint -> Heuristic -> Position -> Position -> Q.PSQ Position Integer -> EstTotalCost -> CostFromStart -> ParentMap -> ParentMap
aStarInternal fp h startPos endPos openSet f c parentMap =
  case mostPromising of
    Nothing -> parentMap --if the open set is empty, A* should have either found an answer or failed (more likely)
    (Just position) ->
      if (position == endPos)
        then parentMap --A* has succeeded
        else undefined
          -- use inBoundsNeighborsOf fp position
          -- next recursive call should have the binding with key equal to position removed
  where
    mostPromising = fmap Q.key $ Q.findMin openSet --this has type Maybe Position

--need memoization for efficiency, not an immediate priority
--should probably use Manhattan distance as a heuristic
--use inBoundsNeighborsOf
--open set represents the search frontier
--ties should be broken by searching the most recently discovered node first, so that multiple equally viable paths aren't drawn out in parallel

-- f(x) = c(x) + h(x)
----c(x) : the lowest cost path from start to x currently known
------implement as a map with all initial values equal to the number of nodes in the Footprint, as this is an upper bound on path cost
----h(x) : the heuristic estimated cost from x to end

--f(x) values should also be implemented as a map with initialized upper bound values for all nodes

--useful heuristic functions go here
mkManhattanHeuristic :: Position -> Heuristic
mkManhattanHeuristic endPos = manhattanDistance endPos

manhattanDistance :: Position -> Position -> Integer
manhattanDistance pos1@(Position x1 y1) pos2@(Position x2 y2) = deltaX + deltaY
  where
    deltaX = abs $ x1 - x2
    deltaY = abs $ y1 - y2