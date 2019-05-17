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
    [ "Starting Environment",
      show SV.pat,
      "Checking what is at position (1,1): ",
      (show (Map.lookup (Env.Position 1 1) SV.env)),
      "Checking what is at position (5,-3): ",
      (show (Map.lookup (Env.Position 5 (-3)) SV.env)),
      "Showing footprint generated from the environment: ",
      (show SV.footprint),
      "printing neighbors of (3,6): ",
      show $ Env.neighborsOf (Env.Position 3 6),
      "list of points viewable from (2, 5) at low altitude: ",
      show $ viewableFrom (DronePos (Env.Position 2 5) Env.Low)    
    ]
  ) >>
  BE.mainWith myCircle
