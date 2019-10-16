module SpanningLoop where

import Env
import GraphOPRC

import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Sequence as Seq

import Data.Maybe
import Data.Tree

--create a path with the following properties, in order of priority
  --1: every element in the set "toVisit" is on the path
  --2: the path ends where it begins
  --3: the number of moves to elements of "inBounds" \ "toVisit" or to elements of "toVisit" that appear earlier on the path are minimized
  --4: the spanning trees that the paths are built around have many leaves, causing the paths to completely fill a lot of nodes even if cut short
initializeSpanningLoop :: Set.Set Position -> Set.Set Position -> Path
initializeSpanningLoop inBounds toVisit = undefined
  --1:
  --
  --

--the set passed to this function should be all of the NODES that are considered in bounds for a particular graph search application (i.e. already pruned out all non-aligned positions, etc.)
inSetCardinalNeighbors :: Int -> Set.Set Position -> Position -> Seq.Seq Position
inSetCardinalNeighbors hopSize inBoundsNodes = undefined
  where
    hops = cardinalHop hopSize <$> [North, East, South, West]

bfs :: Set.Set Position -> (Position -> Seq.Seq Position) -> Forest Position
bfs toSpan neighborF = undefined

--bfsInternal returns the parent map of single spanning tree (not a forest!)
bfsInternal :: (Position -> Seq.Seq Position) -> Map.Map Position Position -> Seq.Seq Position -> Map.Map Position Position
bfsInternal neighborF parentMapSoFar queue =
  if (null queue)
    then parentMapSoFar
    else undefined
  where
    discoveredSet = Map.keysSet parentMapSoFar

--function that converts a parent map to a child map
childMap :: Map.Map Position Position -> Map.Map Position (Set.Set Position)
childMap = undefined 

--function that converts a child map and a root into a tree
cmTree :: Map.Map Position (Set.Set Position) -> Position -> Tree Position
cmTree childMap root = Node root childForest
  where
    childForest = fmap (cmTree childMap) childrenList
    childrenList = Set.toList $ fromMaybe Set.empty $ Map.lookup root childMap
