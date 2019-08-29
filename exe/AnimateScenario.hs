module Main where

import qualified AnimateOPRC as AO
import Env
import SampleVals
import WorldState

import System.Environment
import Graphics.Gloss

main :: IO ()
main = simulate AO.windowDisplay white AO.defaultFramerate AO.initModel AO.drawingFunc AO.updateFunc
  --animate AO.windowDisplay white AO.testAnimate