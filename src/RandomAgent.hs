module RandomAgent where

import Policy
import Drone

import Test.QuickCheck

data RandomPolicy = RandomPolicy (Gen Action)

--instance Policy RandomPolicy where
