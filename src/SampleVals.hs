module SampleVals where

import Env

xc :: XCoord
xc = 5

yc :: YCoord
yc = (-3)

pos :: Position
pos = Position xc yc

pat :: Patch
pat = Patch pos Close

pos2 :: Position
pos2 = Position (xc + 1) yc

pat2 :: Patch
pat2 = Patch pos2 Far