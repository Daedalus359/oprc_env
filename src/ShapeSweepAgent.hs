module ShapeSweepAgent where

import GraphOPRC
import Policy
import Drone
import EnvView
import Ensemble
import Env

import Data.Maybe
import qualified Data.Map.Strict as Map

--agents that execute paths based purely on the footprint of the environment

type Directions = [Action]

--commands all drones to follow along the same path, obviously inefficient
data LowSweepPolicy = LowSweepPolicy
 {
   getDirections :: Directions
 }
 deriving Show

instance Policy LowSweepPolicy where
  nextMove p@(LowSweepPolicy actions) wv@(WorldView envInfo enStat) = 
    if (allIdle enStat)
      then case actions of
             --tell all drones to do the same next action
             (action : moreActions) -> (nextActions, restOfP)
               where
                nextActions = zip (fmap DroneID [1 .. ]) (take numDrones $ repeat action)
                restOfP = LowSweepPolicy moreActions
                numDrones = numDronesRunning wv

             --use A* to figure out the next path to command, then convert this to directions and start following
             [] -> (nextActions, restOfP)--nextMove (LowSweepPolicy directions) wv
               where
                --a recursive call makes more sense, but trying this way for debug purposes
                (nextActions, restOfP) =
                  if (null directions)
                    then (zip (fmap DroneID [1 .. ]) (take numDrones $ repeat Hover), LowSweepPolicy [])
                    else (zip (fmap DroneID [1 .. ]) (take numDrones $ repeat $ head directions), LowSweepPolicy $ tail directions)
                numDrones = numDronesRunning wv

                directions = fromMaybe [Hover] maybeDirections --eventually do something more interesting?

                maybeDirections :: Maybe Directions
                maybeDirections = maybePath >>= makeDirections
                maybePath = aStar envInfo mkManhattanHeuristic startPos nextPosToVisit

                startPos :: Position
                startPos = groundPos $ snd $ head enStat
                nextPosToVisit = if (null unobservedMap)
                                   then minPos
                                   else fst $ Map.findMin unobservedMap

                unobservedMap = Map.filter (not . isFullyObserved) envInfo
                minPos = fst $ Map.findMin envInfo

      --don't command any actions if at least one drone is still in the middle of acting
      else ([], p)

makeDirections :: Path -> Maybe Directions
makeDirections [] = Just []
makeDirections (pos : []) = Just []
makeDirections (pos1 : rest@(pos2 : path)) = 
  case (toAction (getHop pos1 pos2)) of
    Nothing -> Nothing
    (Just action) -> fmap ((:) action) $ makeDirections rest

--policy should plan a path to the lowest ord - valued patch that has not been fully observed

--a second, "HighSweepPolicy" should probably use the same policy as LowSweep for its "phase 2" behavior, after the high environment has been explored and it has descended again
--highsweepPolicy should use a smart function to query A* for paths to a filtered subset of nodes that make sense to visit (e.g rows 2, 5, 8, etc. with shape caveats)
