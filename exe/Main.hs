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
main = do
  putStrLn "Starting Environment"
  putStrLn $ show SV.pat
  putStrLn "Checking what is at position (1,1): "
  putStrLn (show (Map.lookup (Env.Position 1 1) SV.env))
  putStrLn "Checking what is at position (5,-3): "
  putStrLn (show (Map.lookup (Env.Position 5 (-3)) SV.env))
  putStrLn "Showing footprint generated from the environment: "
  putStrLn (show SV.footprint)
  putStrLn "printing neighbors of (3,6): "
  putStrLn $ show $ Env.neighborsOf (Env.Position 3 6)
  putStrLn "list of points viewable from (2, 5) at low altitude: "
  putStrLn $ show $ viewableFrom (DronePos (Env.Position 2 5) Env.Low)
  BE.mainWith myCircle
