{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE TypeFamilies              #-}

module Main where

--oprc imports
import qualified Env
import MoveCosts
import qualified SampleVals as SV
import Drone

--other oprc dependencies
import qualified Data.Map as Map

--diagrams imports
import qualified Diagrams.Prelude as D
import qualified Diagrams.Backend.SVG.CmdLine as BE

myCircle :: D.Diagram BE.B
myCircle = D.circle 1

main :: IO ()
main = foldr (>>) (return ())
  (fmap putStrLn
    [ "Demonstrating oprc_env on sample data values, see SampleVals.hs for details",
      "\nPretty printing a patch",
      show SV.pat,
      "\nChecking what is at position (1,1): ",
      (show (Map.lookup (Env.Position 1 1) SV.env)),
      "\nChecking what is at position (5,-3): ",
      (show (Map.lookup (Env.Position 5 (-3)) SV.env)),
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
      "\n showing an ensemble's status",
      show SV.ensembleStatus,
      ""
    ]
  ) >>
  BE.mainWith myCircle
