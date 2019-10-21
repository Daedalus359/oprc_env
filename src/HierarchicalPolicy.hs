module HierarchicalPolicy where

import AgentUtils
import Env
import EnvView
import GraphOPRC
import Policy
import SpanningTreeAgent

import qualified Data.Map as Map
import qualified Data.Set as Set
import System.Random

data HighFirstBFSPolicy = HighFirstBFSPolicy AltitudePhase StdGen (Map.Map DTPath Footprint)

instance Policy HighFirstBFSPolicy where
  nextMove pol@(HighFirstBFSPolicy LowSweep _ _) wv = --in this case, just do whatever the AdaptiveBFSPolicy does and repackage
    (nextMoves, repackagedPol)
    where
      repackagedPol = fromALBP nextLowPol
      (nextMoves, nextLowPol) = nextMove (toALBP pol) wv

  nextMove (HighFirstBFSPolicy HighSweep gen map) wv@(WorldView envInfo enStat) = undefined --the real novelty of this policy
    where
      currentDronesMap = Map.filterWithKey (droneInSet aliveDrones) map
      aliveDrones = Set.fromList $ fmap fst enStat

removeObservedWaypoints :: HasWaypoints w => EnvironmentInfo -> w -> w
removeObservedWaypoints envInfo w = setWP w $ filter (flip Set.member unseenLocs) $ getWP w
  where
    unseenLocs = unseenLocations envInfo

--the HighFirstBFSPolicy that is functionally identical to the given ALBP
fromALBP :: AdaptiveLowBFSPolicy -> HighFirstBFSPolicy
fromALBP (AdaptiveLowBFSPolicy gen map) = HighFirstBFSPolicy LowSweep gen map

--this will ignore the altitude phase, and then fromALBP will always return one in low mode
toALBP :: HighFirstBFSPolicy -> AdaptiveLowBFSPolicy
toALBP (HighFirstBFSPolicy _ gen map) = AdaptiveLowBFSPolicy gen map

--useless except for testing purposes, functionally identical to the AdaptiveLowBFSPolicy version
initializeLowImmediately :: Int -> StdGen -> WorldView -> HighFirstBFSPolicy
initializeLowImmediately i g w = fromALBP $ initializeALBP i g w