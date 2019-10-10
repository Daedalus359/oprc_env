module Main where

import qualified AnimateReplay as AR
import Env
import qualified SampleVals as SV
import WorldState
import Scenario
import Policy
import EnvView
import ShapeSweepAgent

import qualified System.Environment as SysEnv
import Graphics.Gloss
import RandomOPRC
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

import Data.Text.Prettyprint.Doc
import Data.Text.Prettyprint.Doc.Util

windowWidth = 3400
windowHeight = 2000

edgeRelief = 100 :: Float

windowDisplay :: Display
windowDisplay = InWindow "Window" (windowWidth, windowHeight) (50, 50)

defaultFramerate :: Int
defaultFramerate = 60

--turns a number of seconds elapsed into a number of sim timesteps elapsed
speedupFactor :: Float
speedupFactor = 500

--modify this to calculate the width of the scenario and pass a value based on that to drawReplay as the offset argument
visualReplay :: Scenario p -> IO ()
visualReplay sc = do
                    putStrLn "Number of Patches"
                    print (Set.size fp)
                    simulate windowDisplay white defaultFramerate initModel drawF updateF
  where
    updateF = (AR.updateFunc speedupFactor)
    drawF = (AR.drawReplay2 scaleFactor edgeRelief (fromIntegral windowWidth) (fromIntegral windowHeight) offset)

    offset = fpWidth + 1
    initModel = (0, replay)
    replay = createReplay sc

    scaleFactor = min horizontalScaleFactor vertScaleFactor

    horizontalScaleFactor = horizontalRoom / (2 * (fromIntegral fpWidth) + 1) --map is displayed twice horizontally + 1 column of blank space
    horizontalRoom = ((fromIntegral windowWidth) - (2 * edgeRelief)) / 24

    vertScaleFactor = verticalRoom / (fromIntegral fpHeight)
    verticalRoom = ((fromIntegral windowHeight) - (2 * edgeRelief)) / 24
    fpHeight = maxy - miny + 1

    (miny, maxy) = Set.foldr (\(Position _ y) -> \(minSF, maxSF) -> (min minSF y, max maxSF y)) (sampleY, sampleY) fp

    fpWidth = maxx - minx + 1
    (Position maxx sampleY) = Set.findMax fp
    (Position minx _) = Set.findMin fp

    fp = Map.keysSet $ getMap $ getEnv $ getWorldState sc

main :: IO ()
main = 
  SV.fileNameScenarioWithOutput SV.lkmstp 2 filePath 10000 >>= visualReplay
  --SV.fileNameScenarioWithOutput SV.lstp 1 filePath 100000 >>= visualReplay
  --SV.fileNameScenarioWithOutput SV.kmp 3 filePath 100000 >>= visualReplay
  --SV.fullScenarioWithOutput SV.kmp 6 8 100000 >>= visualReplay
  --SV.fullScenarioWithOutput (return $ const SV.lsPolicy) 6 7 100000 >>= visualReplay
  where
    filePath = "./test/environments/generated/clumpedNE=0.15T=0.5.env"
    --filePath = "./test/environments/spanningTreeTester.env"

quickDraw :: (a -> Picture) -> a -> IO ()
quickDraw f a = display windowDisplay white $ f a