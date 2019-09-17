module AnimateOPRC where

import Drone
import Ensemble
import Env
import EnvView
import WorldState
import Scenario

import Graphics.Gloss
import Graphics.Gloss.Data.ViewPort
import qualified Data.Map.Strict as Map

gridScale :: Float  
gridScale = 24

--the current simulation time that gloss uses, then everything else
type SimState = (Float, ScenarioReplay)

--probably want to just draw the static environment once, and calculate all relative positions based on the size of that
--drawingFunc :: Float -> Picture

drawReplay2 :: Float -> Float -> Float -> Float -> Int -> SimState -> Picture
drawReplay2 scaleFactor edgeRelief width height offset (f, sr) = alignCorners $ scale $ cornerAtZero $ drawReplay offset sr
  where
    alignCorners = Translate (width * (-0.5) + edgeRelief) (height * (-0.5) + edgeRelief)
    scale = Scale scaleFactor scaleFactor
    cornerAtZero = Translate (gridScale / 2) (gridScale / 2)

drawReplay :: Int -> ScenarioReplay -> Picture
drawReplay offset sr@(ScenarioReplay ws time _) = Pictures [drawWorldState offset ws, Text $ show time]

updateFunc :: Float -> ViewPort -> Float -> SimState -> SimState
updateFunc speedupFactor _ dt (t, sr) = (newTime, advanceUntilTime (round newTime) sr)
  where newTime = t + (dt * speedupFactor)

drawTime :: Integer -> Picture
drawTime t = Scale 0.2 0.2 $ Text $ timeString
  where timeString = "Time: " ++ (show t)

frameBarPic :: Picture
frameBarPic = Line [(-d, 0.0), (d, 0.0)]
  where d = gridScale / 4

rotorCircle :: Picture
rotorCircle = Circle (gridScale / 8)

halfDrone :: Picture
halfDrone = Pictures [frameBarPic, Translate d 0.0 rotorCircle, Translate (-d) 0.0 rotorCircle]
  where d = 3 * gridScale / 8

otherHalf :: Picture
otherHalf = Rotate 90 halfDrone

dronePic :: Picture
dronePic = Pictures 
 [ droneHighlightSquare
 , Rotate 45 $ Pictures [halfDrone, otherHalf]
 ]

droneHighlightSquare :: Picture
droneHighlightSquare = Color (withAlpha 0.5 white) filledSquare

squarePath :: Path
squarePath  = [(-d, -d), (-d, d), (d, d), (d, -d)]
  where d = gridScale * 0.5

tripath :: Path
tripath = [(-d, -d), (d, -d), (d, d)]
  where
    d = gridScale * 0.5

halfSquareDiagonal :: Picture
halfSquareDiagonal = Polygon tripath

classifiedMysteryFog :: Picture
classifiedMysteryFog = Pictures [Color (greyN 0.5) halfSquareDiagonal, Line tripath]

filledSquare :: Picture
filledSquare = Polygon squarePath

squareFrame :: Picture
squareFrame = Line squarePath

gridSquare :: Color -> Picture
gridSquare c = Pictures [Color c filledSquare, squareFrame]

drawEnvInfo :: EnvironmentInfo -> Picture
drawEnvInfo = Map.foldMapWithKey (placeAndDraw patchInfoPic)

drawEnv :: Environment -> Picture
drawEnv (Environment envMap) = Map.foldMapWithKey (placeAndDraw patchPic) envMap

drawDroneStat :: DroneStatus -> Picture
drawDroneStat ds = toGridPos x y $ droneFrom alt
  where
    dPos@(DronePos pos@(Position x y) alt) = posFromStat ds

droneFrom :: Altitude -> Picture
droneFrom High = dronePic
droneFrom Low = Scale 0.5 0.5 dronePic

drawEnStat :: EnsembleStatus -> Picture
drawEnStat = foldMap (drawDroneStat . snd)

drawWorldView :: WorldView -> Picture
drawWorldView (WorldView envInfo enStat) = (drawEnvInfo envInfo) <> (drawEnStat enStat)

--draws the state of the world once with full information, then again with only the partil information available to the ensemble
--make this adapt the positioning to the size of the images, or just have it take a parameter that is passed in once from above
drawWorldState :: Int -> WorldState -> Picture
drawWorldState offset ws@(WorldState env envInfo enStat) =
  Pictures
    [ drawEnv env, drawEnStat enStat
    , Translate (gridScale * (fromIntegral offset)) 0 $ drawWorldView wv
    ]

  where
    wv = WorldView envInfo enStat

placeAndDraw :: (a -> Picture) -> Position -> a -> Picture
placeAndDraw f pos@(Position x y) a = toGridPos x y $ f a

patchPic :: Patch -> Picture
patchPic (Patch Close) = gridSquare $ makeColorI 50 200 100 200
patchPic (Patch Far) = gridSquare $ makeColorI 0 75 200 200

patchInfoPic :: PatchInfo -> Picture
patchInfoPic Unseen = Pictures [gridSquare $ greyN 0.5, Translate (-4) (-5) $ Scale 0.1 0.1 $ Text "?"]
patchInfoPic (Classified dist) = Pictures [patchPic $ Patch dist, classifiedMysteryFog]
patchInfoPic (FullyObserved patch) = patchPic patch
--patchInfoPic 

toGridPos :: XCoord -> YCoord -> Picture -> Picture
toGridPos xc yc = Translate (fromIntegral xc * gridScale) (fromIntegral yc * gridScale)

displayPic :: Picture
displayPic = Scale 3 3 $ Pictures 
  [ toGridPos 0 0 $ patchInfoPic $ FullyObserved (Patch Close), toGridPos 0 0 $ droneFrom Low
  , toGridPos 1 0 $ patchInfoPic $ Classified Close
  , toGridPos 1 1 $ patchInfoPic $ FullyObserved (Patch Far), toGridPos 1 1 $ droneFrom High
  , toGridPos 0 1 $ patchInfoPic Unseen
  ]