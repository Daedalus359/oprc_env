module GraphOPRC where

import Control.Monad
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

assignPenalty :: Int -> Altitude -> PatchInfo -> Int
assignPenalty _ _ Unseen = 0
assignPenalty penaltySize _ (FullyObserved _) = penaltySize
assignPenalty penaltySize droneAlt (Classified _) =
  case droneAlt of
    Low -> 0
    High -> penaltySize

aStarStandardPenalty :: Altitude -> EnvironmentInfo -> (Position -> Heuristic) -> Position -> Position -> Maybe Path
aStarStandardPenalty droneAlt envInfo hFunc startPos endPos = aStarCustomPenalty penaltyF envInfo hFunc startPos endPos
  --recreatePath startPos endPos $ aStarInternal penaltyF envInfo fp (hFunc endPos) startPos endPos openSet Set.empty fMax cMax (Map.empty :: ParentMap)
  where
    penaltyF = assignPenalty 9 droneAlt
    --fp = toFootprint envInfo
    --openSet = (Q.singleton startPos 0 :: Q.PSQ Position Int)
    --fMax = initializeETC fp
    --cMax = initializeCFS fp

--"pure" A* just uses the actual move costs and creates a path to minimize that
aStar :: EnvironmentInfo -> (Position -> Heuristic) -> Position -> Position -> Maybe Path
aStar envInfo hFunc startPos endPos = aStarCustomPenalty (const 0) envInfo hFunc startPos endPos

--one that ignores any information about the environment completely by just passing all unknown values
--a little sloppy
aStarByFootprint :: Footprint -> (Position -> Heuristic) -> Position -> Position -> Maybe Path
aStarByFootprint fp hFunc startPos endPos = aStar sampleEnvInfo hFunc startPos endPos
  where
    sampleEnvInfo = Map.fromSet (const Unseen) fp

aStarCustomPenalty :: (PatchInfo -> Int) -> EnvironmentInfo -> (Position -> Heuristic) -> Position -> Position -> Maybe Path
aStarCustomPenalty penaltyF envInfo hFunc startPos endPos = 
  recreatePath startPos endPos $ aStarInternal penaltyF envInfo fp (hFunc endPos) startPos endPos openSet Set.empty fMax cMax (Map.empty :: ParentMap)
  where
    openSet = (Q.singleton startPos 0 :: Q.PSQ Position Int)
    fMax = initializeETC fp
    cMax = initializeCFS fp
    fp = toFootprint envInfo

--continue making recursive calls until endPos can be entered into the ParentMap
aStarInternal :: (PatchInfo -> Int) -> EnvironmentInfo -> Footprint -> Heuristic -> Position -> Position -> Q.PSQ Position Int -> Set.Set Position -> EstTotalCost -> CostFromStart -> ParentMap -> ParentMap
aStarInternal penaltyF envInfo fp h startPos endPos openSet closedSet f c parentMap =
  case mostPromising of
    Nothing -> parentMap --if the open set is empty, A* should have either found an answer or failed (more likely)
    (Just position) ->
      if (position == endPos)
        then parentMap --A* has succeeded
        else aStarInternal penaltyF envInfo fp h startPos endPos newOpenSet newClosedSet newF newC newParentMap
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
            penalizeAccum pos geomCost map = Map.adjust (const $ penaltyF pi + geomCost) pos map
              where
                pi = fromMaybe Unseen $ Map.lookup pos envInfo

            --tune this!
            

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

--this is suitable for use with low flying drones, as it assigns territory based on the incompleteLocations criteria
kMeansAlt :: HasDroneTerritory d => Altitude -> Int -> StdGen -> EnvironmentInfo -> SQ.Seq d -> Map.Map d Footprint
kMeansAlt alt iterations gen envInfo droneSeq = kMeansInternal keepInF nextGen envInfo iterations initMap
  where
    keepInF = case alt of
      Low -> incompleteLocations
      High -> coarseQuadrantCentersFullNodes 3 . (unseenLocations)--this is probably too specific
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
kMeansInternal :: HasDroneTerritory d => (EnvironmentInfo -> Footprint) -> StdGen -> EnvironmentInfo -> Int -> Map.Map d Footprint -> Map.Map d Footprint
kMeansInternal _ _ _ 0 map = map
kMeansInternal incompleteF gen envInfo iterations map = kmiByFootprint gen placesNeedingObservation iterations map
  where
    placesNeedingObservation = incompleteF envInfo --the set of locations that have not been explored fully so far

kmiByFootprint :: HasDroneTerritory d => StdGen -> Footprint -> Int -> Map.Map d Footprint -> Map.Map d Footprint
kmiByFootprint _ _ 0 map = map
kmiByFootprint gen placesNeedingObservation iterations map = kmiByFootprint nextGen placesNeedingObservation (iterations - 1) newMap
  where
    --if any means have lost all of their territory, give them a random patch
    newMap = foldr (\(mean, backupTerritory) -> \existing -> Map.insert mean backupTerritory existing) correctedMeans backupAssignments
      where
        --backupAssignments = zipWith littleTup newTerritories $ Set.toList leftOutMeans --a random location assigned to each of the original means that did not turn into something with territory
        backupAssignments = catMaybes $ fmap closestWVPos $ Set.toList leftOutMeans
        littleTup pos mean = (moveCenter pos mean, Set.singleton pos)

    closestWVPos mean =
      if (null placesNeedingObservation)
        then Nothing
        else Just (mean, Set.singleton newPatch)
      where
        --can probably do better than closerTo by accounting for how many patches belong to the drone that currently owns this territory and travel time
        newPatch = foldr (closerTo currentMean) minPNOPos placesNeedingObservation --this should not fail usually
        DroneTerritory drone currentMean = getDT mean

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
    minPNOPos = Set.findMin placesNeedingObservation

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
  --, getDirsDT :: Directions
  }
  deriving (Eq, Show)

class (Ord d, HasCenter d) => HasDroneTerritory d where
  getDT :: d -> DroneTerritory
  setDT :: d -> DroneTerritory -> d

instance HasDroneTerritory DroneTerritory where
  getDT = id
  setDT = flip const

instance HasDroneTerritory DTDirs where
  getDT (DTDirs dt dirs) = dt
  setDT (DTDirs _ dirs) dt = DTDirs dt dirs--it probably makes sense to preserve dirs in case a kMeans gets run while other drones are acting

data DTDirs = DTDirs DroneTerritory Directions
  deriving (Eq, Show)

class HasCachedDirs c where
  getCachedDirs :: c -> Directions
  setCachedDirs :: c -> Directions -> c

instance HasCachedDirs DTDirs where
  getCachedDirs (DTDirs dt dirs) = dirs
  setCachedDirs (DTDirs dt _) dirs = (DTDirs dt dirs)

--still has a directions spot to cache A* directions to the next path, should be a very short list in most cases
data DTPath = DTPath DroneTerritory Path Directions
  deriving (Eq, Show)

instance HasCenter DTPath where
  getCenter (DTPath dt path dirs) = getCenter dt
  moveCenter newCenter (DTPath dt path dirs) = DTPath (moveCenter newCenter dt) path dirs


instance HasDroneTerritory DTPath
  where
    getDT (DTPath dt _ _) = dt
    setDT (DTPath _ path dirs) dt = DTPath dt path dirs 

class HasWaypoints w where
  getWP :: w -> Path
  setWP :: w -> Path -> w

instance HasWaypoints DTPath where
  getWP (DTPath _ path _)  = path
  setWP (DTPath dt _ dirs) path = DTPath dt path dirs

instance Ord DTPath where
  compare (DTPath dt1 _ _) (DTPath dt2 _ _) = compare dt1 dt2

instance Ord DroneTerritory where
  compare (DroneTerritory d1 _) (DroneTerritory d2 _) = compare d1 d2

instance Ord DTDirs where
  compare dtd1 dtd2 = compare (getDT dtd1) (getDT dtd2)

anyWaiting :: EnsembleStatus -> Set.Set DTDirs -> Bool
anyWaiting enStat territories = getAny $ foldMap (idleAndUndirected enStat) territories

idleAndUndirected :: EnsembleStatus -> DTDirs -> Any
idleAndUndirected enStat dtd@(DTDirs dt@(DroneTerritory drone mean) dirs) =
  case dirs of
    (action : actions) -> Any False
    [] -> if (idleOrUnlisted enStat dt)
            then Any True
            else Any False

idleOrUnlisted :: EnsembleStatus -> DroneTerritory -> Bool
idleOrUnlisted enStat (DroneTerritory drone _) = (fromMaybe True $ fmap isUnassigned $ lookup drone enStat)

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
  moveCenter newMean (DroneTerritory drone _) = DroneTerritory drone newMean

instance HasCenter DTDirs where
  getCenter = getCenter . getDT
  moveCenter newMean (DTDirs dt dirs) = DTDirs (moveCenter newMean dt) dirs

--not all of these locations exist in the fine graph, but something in their "chunk" will
coarseMap :: Int -> Footprint -> Set.Set Position 
coarseMap squareDim fp = foldr checkAndAdd Set.empty fp
  where
    checkAndAdd :: Position -> Set.Set Position -> Set.Set Position
    checkAndAdd pos set =
      if (Set.member (alignedPos pos) set)
        then set
        else Set.insert (alignedPos pos) set

    alignedPos pos = align squareDim pos

coarseMapWithOffset :: Int -> Int -> Int -> Footprint -> Set.Set Position
coarseMapWithOffset squareDim xOffset yOffset boundsFP = foldr checkAndAdd Set.empty boundsFP
  where
    checkAndAdd pos set =
      if (Set.member (alignedPos pos) set)
        then set
        else Set.insert (alignedPos pos) set

    alignedPos pos = hopFrom (align squareDim pos) (xOffset, yOffset)


coarseQuadrantCenters :: Int -> Footprint -> Set.Set Position
coarseQuadrantCenters quadSize boundsFP = coarseMapWithOffset quadSize os os boundsFP
  where
    os = middleHop quadSize


--takes a position and gives back the node corner that "owns" it
align :: Int -> Position -> Position
align squareDim (Position xc yc) = Position (f xc) (f yc)
  where
    f x = x - (mod x squareDim)


--this version gets rid of any centers whose members aren't all in bounds
coarseMapFullNodes :: Int -> Footprint -> Set.Set Position
coarseMapFullNodes squareDim fp = Set.filter allInBoundsMembers $ coarseMap squareDim fp
  where
    allInBoundsMembers :: Position -> Bool
    allInBoundsMembers pos = getAll $ foldMap All boundsList
      where
        boundsList = fmap (\x -> Set.member x fp) $ Set.toList $ nodeSetFromCorner squareDim pos

coarseQuadrantCentersFullNodes :: Int -> Footprint -> Set.Set Position
coarseQuadrantCentersFullNodes quadSize fp = Set.filter allInBoundsMembers $ coarseQuadrantCenters quadSize fp
  where
    allInBoundsMembers :: Position -> Bool
    allInBoundsMembers pos = getAll $ foldMap All boundsList
      where
        boundsList = fmap (\x -> Set.member x fp) $ Set.toList $ quadSetFromCenter quadSize pos


--given a full footprint and the subset of nodes we want included, produce a fully fleshed out set of in bounds positions that belong to those nodes
detailedSetFromNodeCorners :: Int -> Footprint -> Set.Set Position -> Set.Set Position
detailedSetFromNodeCorners squareDim fp cornerSet = Set.fromList $ filter (\p -> Set.member p fp) candidateList
  where
    candidateList = join $ fmap (Set.toList . (nodeSetFromCorner squareDim)) setList
    setList = Set.toList cornerSet

detailedSetFromQuadCenters :: Int -> Footprint -> Set.Set Position -> Set.Set Position
detailedSetFromQuadCenters quadSize fp centerSet = Set.fromList $ filter (\p -> Set.member p fp) candidateList
  where
    candidateList = join $ fmap (Set.toList . (quadSetFromCenter quadSize)) setList
    setList = Set.toList centerSet

--all of the neighbors in a coarse version of the graph
coarseNeighbors :: Int -> Position -> [Position]
coarseNeighbors squareDim (Position xc yc) = tail $ fmap Position (fmap (+ xc) changes) <*> (fmap (+ yc) changes)
  where
    changes = [0, (-squareDim), (squareDim)]

--this specific hop ordering should promote trees with mostly cardinal direction edges
--although a child node will move diagonally to explore a node that the parent could have explored cardinally
--above based on the order that foldr evaluates in dfsInternal
coarseNeighbors2 :: Int -> Position -> [Position]
coarseNeighbors2 squareDim pos = fmap (hopFrom pos) $ zip xChanges yChanges
  where
    yChanges = fmap (* squareDim) yDirs
    xChanges = fmap (* squareDim) xDirs
    (xDirs, yDirs) = unzip [ ( 1,  1) --NE
                           , (-1,  1) --NW
                           , (-1, -1) --SW
                           , ( 1, -1) --SE
                           , ( 1,  0) --E
                           , ( 0, -1) --S
                           , (-1,  0) --W
                           , ( 0,  1) --N
                           ]

--if it is possible to do so, this will guide the creation of a tree with only cardinal edges
coarseCardinalNeighbors :: Int -> Position -> [Position]
coarseCardinalNeighbors squareDim pos = fmap (hopFrom pos) $ zip xChanges yChanges
  where
    yChanges = fmap (* squareDim) yDirs
    xChanges = fmap (* squareDim) xDirs
    (xDirs, yDirs) = unzip [ ( 0, -1) --S
                           , ( 0,  1) --N
                           , (-1,  0) --W
                           , ( 1,  0) --E
                           ]

--if you are at the center of a quadrant, this moves you to some other quadrant (maybe not the same node / square)
quadrantNeighborTo :: Int -> CardinalDir -> Position -> Position
quadrantNeighborTo squareDim dir pos =
  hopFrom pos coarseHop
  where
    quadHop = quot squareDim 2
    coarseHop = case dir of
      North -> (0, quadHop)
      South -> (0, (-quadHop))
      East -> (quadHop, 0)
      West -> ((-quadHop), 0)

--get one of the specific coarse neighbors for a Position
coarseCardinalNeighborTo :: Int -> CardinalDir -> Position -> Position
coarseCardinalNeighborTo squareDim dir pos =
  hopFrom pos coarseHop
  where
    coarseHop = case dir of
      North -> (0, squareDim)
      South -> (0, (-squareDim))
      East -> (squareDim, 0)
      West -> ((-squareDim), 0)

--don't run this on an empty set
dfsSpanningForest :: Int -> Set.Set Position -> Forest Position
dfsSpanningForest squareDim set = customRootDfsSpanningForest squareDim root set
  where
    root = Set.findMin set

--don't feed this a root outside of the bounds set
--only the last of the spanning trees discovered by this algorithm has the specified root
customRootDfsSpanningForest :: Int -> Position -> Set.Set Position -> Forest Position
customRootDfsSpanningForest squareDim root set =
  if (null set) --is there nothing to do?
    then [] --prevent an algorithm from being called that would construct an empty Tree
    else if (null unexplored) --did the first tree span the entire set?
      then [cardinalTree]
      else restOfForest ++ [cardinalTree]


  --OLD VERSION
  --if (cardinalSetSize == desiredSetSize) --if the cardinal moves only algorithm found a valid solution
    --then cardinalTree --then keep that solution
    --else fst $ dfsInternal set neighborF root discoveredSet --otherwise find a solution that uses diagonals

  where
    unexplored = Set.difference set cardinalSet --what was not possible to visit using only cardinal moves from the specified root?

    restOfForest = dfsSpanningForest squareDim unexplored --this gets used only if cardinalTree failed to cover the whole set

    --cardinalSetSize = Set.size cardinalSet
    --desiredSetSize = Set.size set

    (cardinalTree, cardinalSet) = dfsInternal (coarseMap 2 set) cardinalNeighborF root discoveredSet

    neighborF = coarseNeighbors2 squareDim
    cardinalNeighborF = coarseCardinalNeighbors squareDim

    discoveredSet = Set.empty

--dfsInternal and its fold function are mutually recursive
dfsInternal :: Set.Set Position -> (Position -> [Position]) -> Position -> Set.Set Position -> (Tree Position, Set.Set Position)
dfsInternal boundsSet neighborF current inTreeSet = foldr (processNeighbors boundsSet neighborF) (newSeed, newInTreeSet) candidateNeighbors
  where
    newInTreeSet = Set.insert current inTreeSet
    newSeed = (Node current []) --a tree, something for the fold to fill in with child values

    --inBoundsNeighbors = filter (\pos -> Set.member pos boundsSet) candidateNeighbors
    candidateNeighbors = neighborF current --all of the positions that *might* be reachable from current

--use foldr to pass state around
--fold over all of the candidate neighbors, assume access to the overall footprint
processNeighbors :: Set.Set Position -> (Position -> [Position]) -> Position -> (Tree Position, Set.Set Position) -> (Tree Position, Set.Set Position)
processNeighbors boundsSet neighborF childCandidate status@((Node current subTreeList), inTreeSet) =
  --is the position boing considered out of bounds? Alternatively, is it already in the tree?
  if (Set.notMember childCandidate boundsSet || Set.member childCandidate inTreeSet)

    then status --skip the current childCandidate, it does not need to be added to the tree

    --once this evaluates, childCandidate and all of its descendants have been added to the tree
    else (Node current (newSubTree : subTreeList), newInTreeSet) 

  where
    (newSubTree, newInTreeSet) = dfsInternal boundsSet neighborF childCandidate inTreeSet --(Set.insert childCandidate inTreeSet)


--constructs the coarse map and performs DFS on it, bundling the two functions together
customRootDfsFromFootprint :: Int -> Footprint -> Position -> Forest Position
customRootDfsFromFootprint squareDim realFP root = customRootDfsSpanningForest squareDim root coarseFP
  where
    coarseFP = coarseMap squareDim realFP

--same as above but with a default root
minRootDfsFromFootprint :: Int -> Footprint -> Forest Position
minRootDfsFromFootprint squareDim realFP = customRootDfsSpanningForest squareDim root coarseFP
  where
    root = Set.findMin coarseFP
    coarseFP = coarseMap squareDim realFP --so what we pass as a root is gauaranteed to be in the set that dfs explores

--a testing utility for evaluating the performance of my spanning forest algorithms
displayForestCustomRoot :: Int -> Position -> Footprint -> IO ()
displayForestCustomRoot squareDim root fp = putStrLn $ drawForest $ fmap (fmap show) forest
  where
    forest = customRootDfsFromFootprint squareDim fp root

displayForestMinRoot :: Int -> Footprint -> IO ()
displayForestMinRoot squareDim fp = displayForestCustomRoot squareDim root fp
  where
    root = Set.findMin coarseFP
    coarseFP = coarseMap squareDim fp --just so what we pass as a root is gauaranteed to be in the set that dfs explores

--comes up with a path through each quadrant (squares with side length = 0.5 * squareDim)
  --so don't run on spanning trees whose squareDim parameter was odd
--should be run on forests with only cardinal edges
cardinalCoveragePath :: Int -> Forest Position -> Path
cardinalCoveragePath squareDim forest = foldr (treePathAccumulator squareDim) [] forest

minRootSpanningTreeCoveragePath :: Int -> Footprint -> Path
minRootSpanningTreeCoveragePath squareDim fp = customRootSpanningTreeCoveragePath squareDim fp root
  where
    root = Set.findMin coarseFP
    coarseFP = coarseMap squareDim fp

customRootSpanningTreeCoveragePath :: Int -> Footprint -> Position -> Path
customRootSpanningTreeCoveragePath squareDim fp root = cardinalCoveragePath squareDim forest
  where
    forest = customRootDfsFromFootprint squareDim fp root

treePathAccumulator :: Int -> Tree Position -> Path -> Path
--the order in which the paths are combined should make the front of the forest contribute to the end of the path
  --that's good because the biggest tree should be the first one made in most cases, and that goes at the end of the forest
treePathAccumulator squareDim newTree prevPath = prevPath ++ (spanningTreePath squareDim newTree)

--comes up with a coverage path than traverses a single spanning tree
--parent and child nodes should be coarse cardinal neighbors
spanningTreePath :: Int -> Tree Position -> Path
spanningTreePath squareDim node@(Node rootCorner _) =
  --rootCorner is the lower left position in the "territory" (squareDim ^ 2 positions) assigned to it
  (:) quadCenter $ stpInternal quadHop quadCenter node
  where
    quadCenter = centerPos squareDim rootCorner
    quadHop = quot squareDim 2 --hopping by (+/-quadHop, +/-quadHop) moves between the center of one quadrant to the center of another

--where the real tree following starts
stpInternal :: Int -> Position -> Tree Position -> Path
--case where there are 0 or more children to visit. As with all calls to stpInternal, this call should only be made when we have just moved in to the current node. Need to cycle through children in a principled order, moving in, doing a recursive call, and moving back out for each child. Need to move between quadrants directly whenever there is no edge to what would otherwise be the next appropriate child to visit. Accomplish all of this with a fold.
stpInternal quadHop currentPos (Node cornerPos childTrees) =
  --init drops the very last element because that always loops around one step too far
  init $ foldr (visitChildren squareDim cornerPos childTrees) [] prioritizedDirections
  where
    prioritizedDirections = take 4 $ drop dropNum $ cycle [South, East, North, West]
    dropNum = case (whichQuadrant quadHop currentPos llQuadCenter) of
      BottomLeft -> 0
      BottomRight -> 1
      TopRight -> 2
      TopLeft -> 3
    llQuadCenter = centerPos squareDim cornerPos
    squareDim = 2 * quadHop

visitChildren :: Int -> Position -> Forest Position -> CardinalDir -> Path -> Path 
visitChildren squareDim cornerPos childTrees direction restOfFold = newPath ++ restOfFold --tricky ordering, see foldr definition
  where
    newPath = if (elem possibleChild children)
      then (nextPos : ( recursivePath ++ [newPos])) --nextPos moves into the first quadrant of the recursive call, newPos moves it back out
      else [newPos]
    recursivePath = stpInternal quadHop nextPos childTree
    childTree = childTrees !! childIndex
    childIndex = fromMaybe 0 $ findIndex (\tree -> rootLabel tree == childCorner) childTrees
    childCorner = coarseCardinalNeighborTo squareDim direction cornerPos
    nextPos = quadrantNeighborTo squareDim direction currentPos--where we start when recursion is called
    newPos = hopFrom currentPos cycleHop --for when we are just moving through the current square
    children = roots childTrees
    (_, cycleHop) = cycleQuads quadHop currentQuad
    currentPos = quadPos squareDim cornerPos currentQuad
    quadHop = quot squareDim 2
    currentQuad = case direction of
      South -> BottomLeft
      East -> BottomRight
      North -> TopRight
      West -> TopLeft
    possibleChild = coarseCardinalNeighborTo squareDim direction cornerPos

data Quadrant = BottomLeft | BottomRight | TopLeft | TopRight
  deriving (Eq, Show)

roots :: Forest a -> [a]
roots trees = foldr treeRootAccum [] trees
  where
    treeRootAccum :: Tree a -> [a] -> [a]
    treeRootAccum (Node aVal children) soFar = aVal : soFar

--the path to move from the quadrant you are in to the one where you want to be
quadrantPath :: Int -> Quadrant -> Position -> Quadrant -> Path
quadrantPath quadHop startQuad startPos endQuad =
  if (startQuad == endQuad)
    then []
    else nextPos : (quadrantPath quadHop nextQuad nextPos endQuad)
  where
    nextPos = hopFrom startPos nextHop
    (nextQuad, nextHop) = cycleQuads quadHop startQuad


--if you are moving in a square's internal cycle, this tells you:
  --1: where you are going next
  --2: the hop that will get you there
cycleQuads :: Int -> Quadrant -> (Quadrant, Hop)
cycleQuads quadHop BottomLeft = (BottomRight, (quadHop, 0))
cycleQuads quadHop BottomRight = (TopRight, (0, quadHop))
cycleQuads quadHop TopRight = (TopLeft, ((-quadHop), 0))
cycleQuads quadHop TopLeft = (BottomLeft, (0, (-quadHop))) 

--given a position at the center of a quadrant, and given the center of the lower left quadrant, what quadrant is the position in?
whichQuadrant :: Int -> Position -> Position -> Quadrant
whichQuadrant quadHop pos@(Position xc yc) q1Center@(Position q1cx q1cy) =
  case (dxH, dyH) of
    (0, 0) -> BottomLeft
    (0, 1) -> TopLeft
    (1, 0) -> BottomRight
    (1, 1) -> TopRight
  where
    dxH = quot (xc - q1cx) quadHop
    dyH = quot (yc - q1cy) quadHop
    --undefined for all positions that aren't one of the quadrant centers of the square as defined

--given the lower left coner of a square of size squareDim, find the center of the lower left quadrant
centerPos :: Int -> Position -> Position
centerPos squareDim cornerPos@(Position xc yc) = hopFrom cornerPos (hopSize, hopSize)
  where
    hopSize = middleHop quadrantDim
    quadrantDim = quot squareDim 2 


middleHop :: Int -> Int --how far in each of x and y from the corner of a quadrant of a certain size do you move to get from that corner to the center of the quadrant
--if the quadrant has even size, bias towards being close to the corner
middleHop quadrantDim = d + (min m 1) - 1
  where
    (d, m) = divMod quadrantDim 2

--given the "center" of a quadrant of known size, what is the set of all Positions in that quadrant?
quadSetFromCenter :: Int -> Position -> Set.Set Position
quadSetFromCenter quadSize centerPos = Set.fromList $ fmap Position (fmap (+ xc) changes) <*> (fmap (+ yc) changes)
  where
    changes = [0 .. (quadSize - 1)]
    cornerPos@(Position xc yc) = hopFrom centerPos ((-hopSize), (-hopSize)) --just the inverse of what happens in centerPos

    hopSize = halfQuadrant + (min m 1) - 1 
    (halfQuadrant, m) = divMod quadSize 2

nodeSetFromCorner :: Int -> Position -> Set.Set Position
nodeSetFromCorner squareDim cornerPos = Set.map (hopFrom cornerPos) hops
  where
    hops = Set.fromList $ (,) <$> hopSizes <*> hopSizes
    hopSizes = [0 .. (max 0 $ squareDim - 1)]

--given where the corner is, where is the center of the quadrant we want?
quadPos :: Int -> Position -> Quadrant -> Position
quadPos squareDim cornerPos quad = hopFrom llCorner hop
  where
    hop = case quad of
      BottomLeft -> (0, 0)
      BottomRight -> (quadHop, 0)
      TopLeft -> (0, quadHop)
      TopRight -> (quadHop, quadHop)
    quadHop = quot squareDim 2
    llCorner = centerPos squareDim cornerPos

--given the real footprint and the path built around a coarse cardinal spanning tree, find a path with the following properties
  --1: all positions are in bounds
  --2: all positions from the original path that exist in the environment are kept
  --3: if a position in the original path is out of bounds, the lowest valued in-bounds member of its quadrant is included
  --4: if a path entries entire quadrant is out of bounds, its entry is skipped
    --(note that this is possible because the coarse map looks for any element in the 2x2 quadrant squares)
inBoundsPath :: Int -> Footprint -> Path -> Path
inBoundsPath quadSize fp origPath = join $ zipWith keepOrFindQuadMember inBoundsLabels origPath
  where
    inBoundsLabels = fmap (inBounds fp) origPath

    keepOrFindQuadMember :: Bool -> Position -> [Position]
    keepOrFindQuadMember True original = [original]
    keepOrFindQuadMember False original = coarseQuadVisitList quadSize original commonGround
      where
        --set intersection of the quadrant and the footprint
        commonGround = Set.intersection quadrantSet fp

        --the points that, if in bounds, would be suitable substitutes for original
        quadrantSet = quadSetFromCenter quadSize original

coarseQuadVisitList :: Int -> Position -> Footprint -> [Position]
coarseQuadVisitList quadSize original commonGround = 
  if (null commonGround)
    then []
    else newAddition : (coarseQuadVisitList quadSize original newCommonGround)
  where
    newCommonGround = Set.difference commonGround (quadSetFromCenter quadSize newAddition)
    newAddition = foldr (closerTo original) minPos commonGround
    minPos = Set.findMin commonGround --should be find because it only gets evaluated when commonground is not null

customRootInBoundsSpanningTreePath :: Int -> Footprint -> Position -> Path
customRootInBoundsSpanningTreePath squareDim visitFP root = inBoundsPath squareDim visitFP unboundedPath
  where
    unboundedPath = customRootSpanningTreeCoveragePath squareDim visitFP root

minRootInBoundsSpanningTreePath :: Int -> Footprint -> Path
minRootInBoundsSpanningTreePath squareDim visitFP = customRootInBoundsSpanningTreePath squareDim visitFP root
  where
    root = Set.findMin coarseFP
    coarseFP = coarseMap squareDim visitFP