module Main where

import qualified AnimateOPRC as AO
import Env
import qualified SampleVals as SV
import WorldState
import Scenario

import System.Environment
import Graphics.Gloss

--modify this to calculate the width of the scenario and pass a value based on that to drawReplay as the offset argument
visualReplay :: Scenario p -> IO ()
visualReplay sc = simulate AO.windowDisplay white AO.defaultFramerate initModel (AO.drawReplay 16) AO.updateFunc
  where
    initModel = createReplay sc

main :: IO ()
main =
  --simulate AO.windowDisplay white 1 (50 :: Float) (Circle) (\vp -> \dt -> \f -> f + (dt * 10)) >> 
  SV.fullScenarioWithOutput (return SV.lsPolicy) 1 6 100000 >>= visualReplay