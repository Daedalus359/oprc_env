module AnimateOPRC where

import Graphics.Gloss
import Graphics.Gloss.Data.ViewPort

windowDisplay :: Display
windowDisplay = InWindow "Window" (200, 200) (100, 100)

testPic :: Picture
testPic = Circle 80

defaultFramerate :: Int
defaultFramerate = 10

initModel :: Int
initModel = 10

drawingFunc :: Int -> Picture
drawingFunc = Circle . fromIntegral

updateFunc :: ViewPort -> Float -> Int -> Int
updateFunc _ dt i = round dt * 10 + i

test = "."