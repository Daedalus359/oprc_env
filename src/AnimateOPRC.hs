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
gridScale = 20

windowDisplay :: Display
windowDisplay = InWindow "Window" (600, 600) (100, 100)

testAnimate :: Float -> Picture
testAnimate = Circle . ( * 10)

defaultFramerate :: Int
defaultFramerate = 60

initModel :: Float
initModel = 10

drawingFunc :: Float -> Picture
drawingFunc t =
  Rotate (5 * t) $ Scale 3 3 $ drawWorldView wv
  where
    wv = WorldView envInfo enStat
    enStat = 
      [ (DroneID 1, Unassigned (DronePos (Position 0 0) Low))
      , (DroneID 2, Unassigned (DronePos (Position 1 1) High))
      ]
    envInfo = Map.fromList
                  [ (Position 0 0, FullyObserved (Patch Close))
                  , (Position 1 0, Classified Close)
                  , (Position 0 1, Unseen)
                  , (Position 1 1, FullyObserved (Patch Far))
                  ]

updateFunc :: ViewPort -> Float -> Float -> Float
updateFunc _ dt i = dt * 10 + i

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

placeAndDraw :: (a -> Picture) -> Position -> a -> Picture
placeAndDraw f pos@(Position x y) a = toGridPos x y $ f a

patchInfoPic :: PatchInfo -> Picture
patchInfoPic Unseen = Pictures [gridSquare $ greyN 0.5, Translate (-4) (-5) $ Scale 0.1 0.1 $ Text "?"]
patchInfoPic (Classified Close) = Pictures [patchInfoPic (FullyObserved $ Patch Close), classifiedMysteryFog]
patchInfoPic (Classified Far) = Pictures [patchInfoPic (FullyObserved $ Patch Far), classifiedMysteryFog]
patchInfoPic (FullyObserved (Patch Close)) = gridSquare $ makeColorI 50 200 100 200
patchInfoPic (FullyObserved (Patch Far)) = gridSquare $ makeColorI 0 75 200 200
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