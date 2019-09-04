module Main where

import qualified AnimateOPRC as AO
import Env
import qualified SampleVals as SV
import WorldState
import Scenario

import System.Environment
import Graphics.Gloss

import Data.Text.Prettyprint.Doc
import Data.Text.Prettyprint.Doc.Util

--modify this to calculate the width of the scenario and pass a value based on that to drawReplay as the offset argument
visualReplay :: Scenario p -> IO ()
visualReplay sc = simulate AO.windowDisplay white AO.defaultFramerate initModel (AO.drawReplay2 offset) AO.updateFunc
  where
    offset = 27
    initModel = (0, replay)
    replay = createReplay sc

main :: IO ()
main = do
  scenario <- SV.fullScenarioWithOutput (return SV.lsPolicy) 1 8 100000
  putDocW 80 $ pretty $ scenario
  putDocW 80 $ pretty $ advanceUntilTime 51 $ createReplay scenario
  visualReplay scenario
  --simulate AO.windowDisplay white 1 (50 :: Float) (Circle) (\vp -> \dt -> \f -> f + (dt * 10)) >> 
  -- fmap createReplay (SV.fullScenarioWithOutput (return SV.lsPolicy) 1 4 100000) >>= print --visualReplay
  --SV.fullScenarioWithOutput (return SV.lsPolicy) 1 4 100000 >>= visualReplay