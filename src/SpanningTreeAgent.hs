module SpanningTreeAgent where

import Env
import EnvView
import GraphOPRC
import Policy

import System.Random
import qualified Data.Set as Set
import qualified Data.Map as Map

testCompilation = "work!"

--this policy explores a spanning tree that minimizes covering old ground
--in this case, the spanning tree paths live inside the DroneTerritory datatype
data LowSpanningTreePolicy = LowSpanningTreePolicy StdGen (Map.Map DroneTerritory Footprint)

instance Policy LowSpanningTreePolicy where
  nextMove p@(LowSpanningTreePolicy gen map) wv@(WorldView envInfo enStat) = undefined