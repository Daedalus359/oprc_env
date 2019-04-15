{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE TypeFamilies              #-}

module Main where

--oprc imports
import Env
import MoveCosts
import qualified SampleVals as SV

--diagrams imports
import qualified Diagrams.Prelude as D
import qualified Diagrams.Backend.SVG.CmdLine as BE

myCircle :: D.Diagram BE.B
myCircle = D.circle 1

main :: IO ()
main = do
  putStrLn "Starting Environment"
  putStrLn $ show SV.pat

--main = BE.mainWith myCircle
