module Main where

import Env
import MoveCosts
import qualified SampleVals as SV

main :: IO ()
main = do
  putStrLn "Starting Environment"
  putStrLn $ show SV.pat