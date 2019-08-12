{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE TypeFamilies              #-}

module Main where

--oprc imports
import qualified Env
import MoveCosts
import qualified SampleVals as SV
import WorldState
import Drone
import qualified Scenario

--prettyprinting
import PrettyOPRC
import Data.Text.Prettyprint.Doc
import Data.Text.Prettyprint.Doc.Util

--other oprc dependencies
import qualified Data.Map.Strict as Map

--diagrams imports
import qualified Diagrams.Prelude as D
import qualified Diagrams.Backend.SVG.CmdLine as BE

--general utilities
import Util (nTimes)
import Control.Applicative

myCircle :: D.Diagram BE.B
myCircle = D.circle 1

main :: IO ()
main = foldr (>>) (return ())
  (fmap putStrLn
    [ "Demonstrating oprc_env on sample data values, see SampleVals.hs for details",
      "\nPretty printing a patch",
      show SV.pat,
      "\nChecking what is at position (1,1): ",
      (show (Map.lookup (Env.Position 1 1) (Env.toMap SV.env))),
      "\nChecking what is at position (5,-3): ",
      (show (Map.lookup (Env.Position 5 (-3)) (Env.toMap SV.env))),
      "\nShowing footprint generated from the environment: ",
      (show SV.footprint),
      "\nprinting neighbors of (3,6): ",
      show $ Env.neighborsOf (Env.Position 3 6),
      "\nlist of locations viewable from (2, 5) at low altitude: ",
      show $ viewableFrom (DronePos (Env.Position 2 5) Env.Low),
      "\nlist of locations viewable from (2, 5) at high altitude: ",
      show $ viewableFrom (DronePos (Env.Position 2 5) Env.High),
      "\nshowing a drone's status",
      show SV.workingUp,
      "\nshowing an ensemble's status",
      show SV.ensembleStatus,
      "\nshowing partial information about an environment",
      show SV.envInfo,
      "\nshowing a WorldState",
      show SV.worldState,
      "\nshowing the same WorldState after 4 updates (no new actions commanded)",
      show $ nTimes 4 (updateState []) SV.worldState,
      ""
    ]
  ) >>
  --SV.manualControl SV.worldState >>
  --SV.dumpParseFailure2 SV.manualControl (SV.liftToWS 1 $ SV.parseEnvNum 4) >> 
  BE.mainWith myCircle >>
  -- SV.fullScenarioWithOutput SV.randPolicy 1 4 100 >>
  SV.fullScenarioWithOutput SV.randFiltPolicy 4 6 10000 >>
  -- SV.firstStepsWithOutput >> 
  return ()