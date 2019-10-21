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

--if the new toVisit is a superset of the old one, make loops that cover the new patches and merge with the existing loop
--if the new toVisit has lost elements, see if I can figure out a way to "fuse shut" the loops
adaptLoop :: Set.Set Position -> Set.Set Position -> Path -> Path
adaptLoop = undefined

--the set passed to this function should be all of the NODES that are considered in bounds for a particular graph search application (i.e. already pruned out all non-aligned positions, etc.)
inSetCardinalNeighbors :: Int -> Set.Set Position -> Position -> Seq.Seq Position
inSetCardinalNeighbors hopSize inBoundsNodes startPos = 
  Seq.fromList $ filter (\p -> Set.member p inBoundsNodes) $ hopFrom startPos <$> hops
  where
    hops = cardinalHop hopSize <$> [North, East, South, West]

--in this case, visitSet is coarse while boundsSet is detailed
achievableLowNeighbors :: Footprint -> Set.Set Position -> Position -> Seq.Seq Position
achievableLowNeighbors boundsSet visitSet startPos = Seq.fromList reachableWorthVisiting
  where
    reachableWorthVisiting = filter (\p -> Set.member p visitSet) reachableNeighborPositions

    --which neighbors can be visited according to bounds set?
    reachableNeighborPositions = hopFrom startPos <$> (cardinalHop 2 <$> legalMoves)

    --which neighbors can accept a visitor from the direction of startPos
      --and also satisfy the first criteria
    legalMoves = filter (neighborCanReceiveFrom 2 boundsSet startPos) startPosOutgoingDirs
    startPosOutgoingDirs = whichSidesLow boundsSet startPos--which directions can the start position support leaving from?

neighborCanReceiveFrom :: Int -> Footprint -> Position -> CardinalDir -> Bool
neighborCanReceiveFrom hopSize boundsFP startPos dir = elem sideNeeded $ whichSidesLow boundsFP neighborPos
  where
    (neighborHop, sideNeeded) = case dir of
      North -> ((0, hopSize), South)
      South -> ((0, - hopSize), North)
      East -> ((hopSize, 0), West)
      West -> ((- hopSize, 0), East)
    neighborPos = hopFrom startPos neighborHop

--given a 2x2 node with the specified "center" (SW corner), which sides of that node contain at least one in bounds position?
whichSidesLow :: Footprint -> Position -> [CardinalDir]
whichSidesLow fp centerPos = filter hasPos [North, East, South, West]
  where
    hasPos South = swCorner || seCorner
    hasPos North = nwCorner || neCorner
    hasPos East = neCorner || seCorner
    hasPos West = nwCorner || swCorner
    [swCorner, seCorner, nwCorner, neCorner] = (\p -> Set.member p fp) <$> ((hopFrom centerPos) <$> [(0, 0), (1, 0), (0, 1), (1, 1)])

--don't call this with an empty set!
customRootThenMin :: Position -> Set.Set Position -> Position
customRootThenMin cRoot set =
  if (Set.member cRoot set)
    then cRoot
    else Set.findMin set

customRootThenClosest :: Position -> Set.Set Position -> Position
customRootThenClosest cRoot set =
  if (Set.member cRoot set)
    then cRoot
    else foldr (closerTo cRoot) minPos set
  where
    minPos = Set.findMin set


bfs :: Set.Set Position -> (Position -> Seq.Seq Position) -> (Set.Set Position -> Position) -> Forest Position
bfs toSpan neighborF rootF =
  if (Set.null toSpan)
    then []
    else currentTree : (bfs remainingToSpan neighborF rootF)
  where
    currentTree = pmTree root $ Map.delete root firstMap 
    remainingToSpan = Set.difference toSpan $ Map.keysSet firstMap
    root = rootF toSpan
    firstMap = bfsInternal neighborF (Map.singleton root root) (Seq.singleton root)


--bfsInternal returns the parent map of single spanning tree (not a forest!)
--the LEFT end of the queue is where you DEQUEUE from
--the final map will have the root as its own parent, so remove this afterwards
bfsInternal :: (Position -> Seq.Seq Position) -> Map.Map Position Position -> Seq.Seq Position -> Map.Map Position Position
bfsInternal neighborF parentMapSoFar queue =
  if (null queue)
    then parentMapSoFar
    else bfsInternal neighborF newPMap nextQueue

  where
    nextQueue = restOfQueue Seq.>< unvisitedNeighbors
    newPMap = Map.union parentMapSoFar $ foldr (\p -> \newMap -> Map.insert p current newMap) Map.empty unvisitedNeighbors
    unvisitedNeighbors = Seq.filter (\p -> Set.notMember p alreadyVisitedSet) $ neighborF current
    current Seq.:< restOfQueue = Seq.viewl queue
    alreadyVisitedSet = Map.keysSet parentMapSoFar

pmTree :: Position -> Map.Map Position Position -> Tree Position
pmTree root parentMap = cmTree theChildMap root
  where
    theChildMap = (childMap parentMap)

--function that converts a parent map to a child map
childMap :: Map.Map Position Position -> Map.Map Position (Set.Set Position)
childMap parentMap = childMapInternal Map.empty parentMap

childMapInternal :: Map.Map Position (Set.Set Position) -> Map.Map Position Position -> Map.Map Position (Set.Set Position)
childMapInternal cMapSoFar pMap =
  if (null pMap)
    then cMapSoFar
    else childMapInternal nextCMap restOfPMap

  where
    nextCMap = Map.alter (insertOrSingleton newChild) newParent cMapSoFar
    ((newChild, newParent), restOfPMap) = Map.deleteFindMin pMap

insertOrSingleton :: Ord a => a -> Maybe (Set.Set a) -> Maybe (Set.Set a)
insertOrSingleton aVal Nothing = Just $ Set.singleton aVal
insertOrSingleton aVal (Just setSoFar) = Just $ Set.insert aVal setSoFar

--function that converts a child map and a root into a tree
cmTree :: Map.Map Position (Set.Set Position) -> Position -> Tree Position
cmTree childMap root = Node root childForest
  where
    childForest = fmap (cmTree childMap) childrenList
    childrenList = Set.toList $ fromMaybe Set.empty $ Map.lookup root childMap

--a function that uses a coarse map and neighbor function that works a lot like my dfs implementation
lowBFSSpanningTree :: Footprint -> Position -> Forest Position
lowBFSSpanningTree fp root = bfs toSpan neighborF rootF
  where
    rootF = customRootThenClosest root
    neighborF = inSetCardinalNeighbors 2 toSpan
    toSpan = coarseMap 2 fp

lowBFSCoarsePath :: Footprint -> Footprint -> Position -> Path
lowBFSCoarsePath boundsFp visitFp root = inBoundsPath 1 boundsFp $ cardinalCoveragePath 2 forest
  where
    forest = lowBFSSpanningTree visitFp root

smartEdgeBFSForestLow :: Footprint -> Footprint -> Position -> Forest Position
smartEdgeBFSForestLow boundsFP visitSet root = bfs toSpanCoarse neighborF rootF
  where
    rootF = customRootThenClosest root
    neighborF = achievableLowNeighbors boundsFP toSpanCoarse
    toSpanCoarse = coarseMap 2 visitSet

lowSmartEdgeBFSCoarsePath :: Footprint -> Footprint -> Position -> Path
lowSmartEdgeBFSCoarsePath boundsFP visitFP root = inBoundsPath 1 boundsFP $ cardinalCoveragePath 2 forest
  where
    forest = smartEdgeBFSForestLow boundsFP visitFP root