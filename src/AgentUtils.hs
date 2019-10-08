module AgentUtils where

import Env
import Drone
import GraphOPRC

testCompilation = "do it!"

--outputs Directions if all of the hops between path elements correspond to atomic movements in the sim
makeDirections :: Path -> Maybe Directions
makeDirections [] = Just []
makeDirections (pos : []) = Just []
makeDirections (pos1 : rest@(pos2 : path)) = 
  case (toAction (getHop pos1 pos2)) of
    Nothing -> Nothing
    (Just action) -> fmap ((:) action) $ makeDirections rest