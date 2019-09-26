module NeuralNet where

import Data.Vector
import Numeric.AD

--activation functions
reLU :: Float -> Float
reLU x = max x 0

leakyReLU :: Float -> Float
leakyReLU x =
  if (x >= 0)
    then x
    else 0.01 * x

--close enough
eulerNum :: Float
eulerNum = 2.718

sigmoid :: Float -> Float
sigmoid x = 1 / (1  + (eulerNum ** (-1 * x)))

--data Layer 

--network :: 