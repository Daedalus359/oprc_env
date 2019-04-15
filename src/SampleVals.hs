module SampleVals where

import Env
import qualified Data.Map as Map

xc :: Env.XCoord
xc = 5

yc :: Env.YCoord
yc = (-3)

pos :: Env.Position
pos = Position xc yc

pat :: Env.Patch
pat = Patch Close

pos2 :: Env.Position
pos2 = Position (xc + 1) yc

pat2 :: Env.Patch
pat2 = Patch Far

env :: Env.Environment
env  = Map.fromList [(pos, pat), (pos2, pat2)]
