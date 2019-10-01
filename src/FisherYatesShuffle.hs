module FisherYatesShuffle where

import Control.Monad.ST

import qualified Data.Vector as V
import qualified Data.Vector.Mutable as MV

import System.Random

--handle conversion between vector <-> list
shuffle :: StdGen -> [a] -> [a]
shuffle gen xs = V.toList $ runST $ vecShuffle gen maxIndex =<< V.thaw vec 
  where
    maxIndex = V.length vec - 1
    vec = V.fromList xs

vecShuffle :: StdGen -> Int -> V.MVector s a -> ST s (V.Vector a)
vecShuffle _ 0 stv = V.freeze stv
vecShuffle gen maxIndex stv =
  do
    MV.swap stv randomIndex maxIndex --swap a randomly selected value from the currently unassigned vector with the one at the end of that space
    vecShuffle nextGen (maxIndex - 1) stv --do it again to the front of the vector (everything before maxindex)
  where
    (randomIndex, nextGen) = randomR (0, maxIndex) gen