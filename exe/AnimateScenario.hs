module Main where

import qualified AnimateOPRC as AO
import Env
import qualified SampleVals as SV
import WorldState
import Scenario
import Policy
import EnvView
import ShapeSweepAgent

import System.Environment
import Graphics.Gloss
import RandomOPRC

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
main = SV.fullScenarioWithOutput SV.kmp 3 8 100000 >>= visualReplay

--other policy functions
  --(return $ const SV.lsPolicy)