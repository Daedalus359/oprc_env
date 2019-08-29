module Main where

import Env
import WorldState
import qualified AnimateOPRC as AO

import System.Environment
import Graphics.Gloss

main :: IO ()
main = simulate AO.windowDisplay white AO.defaultFramerate AO.initModel AO.drawingFunc AO.updateFunc
  --animate AO.windowDisplay white AO.testAnimate