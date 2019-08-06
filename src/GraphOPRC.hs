module GraphOPRC where

import Env

--allows for efficiently building a Path as a list from back to front
data PathStep = PathStep Position (Maybe ParentPos)

type ParentPos = Position

testCompilation = "!"