module AnimateOPRC where

import Env
import WorldState
import Scenario

import Graphics.Gloss
import Graphics.Gloss.Data.ViewPort

windowDisplay :: Display
windowDisplay = InWindow "Window" (200, 200) (100, 100)

testPic :: Picture
testPic = Circle 80

testAnimate :: Float -> Picture
testAnimate = Circle . ( * 10)

defaultFramerate :: Int
defaultFramerate = 10

initModel :: Float
initModel = 10

drawingFunc :: Float -> Picture
drawingFunc t = Rotate (10 * t) dronePic

updateFunc :: ViewPort -> Float -> Float -> Float
updateFunc _ dt i = dt * 10 + i

frameBarPic :: Picture
frameBarPic = Line [(-10.0, 0.0), (10.0, 0.0)]

rotorCircle :: Picture
rotorCircle = Circle 5

halfDrone :: Picture
halfDrone = Pictures [frameBarPic, Translate 13.0 0.0 rotorCircle, Translate (-13.0) 0.0 rotorCircle]

otherHalf :: Picture
otherHalf = Rotate 90 halfDrone

dronePic :: Picture
dronePic = Pictures [halfDrone, otherHalf]