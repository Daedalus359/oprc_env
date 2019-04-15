{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE TypeFamilies              #-}

module Main where

--oprc imports
import Env
import MoveCosts
import qualified SampleVals as SV

--diagrams imports
import Diagrams.Prelude
import qualified Diagrams.Backend.SVG.CmdLine as BE

myCircle :: Diagram BE.B
myCircle = circle 1

--main :: IO ()
--main = do
  --putStrLn "Starting Environment"
  --putStrLn $ show SV.pat

main = BE.mainWith myCircle
