module FisherYatesShuffle where

import Control.Monad.ST

import qualified Data.Vector as V
import qualified Data.Vector.Mutable as MV

import System.Random

import qualified Control.Monad.Primitive as P

shuffle :: StdGen -> [a] -> [a]
shuffle gen xs = V.toList $ runST $ vecShuffle gen maxIndex $ V.thaw vec
  where
    maxIndex = V.length vec - 1
    vec = V.fromList xs

vecShuffle :: StdGen -> Int -> ST s (V.MVector (P.PrimState (ST s)) a) -> ST s (V.Vector a)
vecShuffle _ 0 stv = stv >>= V.freeze
vecShuffle gen maxIndex stv =
  do
    mVec <- stv
    MV.swap mVec randomIndex maxIndex
    vecShuffle nextGen (maxIndex - 1) (return mVec)
  where
    (randomIndex, nextGen) = randomR (0, maxIndex) gen

sampleList = [0 .. 1000]

demoShuffle :: IO ()
demoShuffle = do
  gen <- newStdGen
  print $ shuffle gen sampleList