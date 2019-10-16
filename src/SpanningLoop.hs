module SpanningLoop where

import Env
import GraphOPRC

import qualified Data.Set as Set
import qualified Data.Sequence as Seq
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




