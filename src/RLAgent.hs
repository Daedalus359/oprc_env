module RLAgent where

import Env
import EnvView
import WorldState

import Data.Monoid

infoValue :: PatchInfo -> Float
infoValue Unseen = -1.0
infoValue (Classified Close) = 0.5
infoValue (Classified Far) = 0.9 --shouldn't come up
infoValue (FullyObserved _) = 1.0

viewValue :: EnvironmentInfo -> Float
viewValue envInfo = getSum $ foldMap (\pi -> Sum $ infoValue pi) envInfo

viewImprovement :: EnvironmentInfo -> EnvironmentInfo -> Float
viewImprovement old new = (viewValue new) - (viewValue old)

reward :: WorldView -> WorldView -> Float
reward old new = (viewImprovement (getView old) (getView new)) - 1.0 -- -1.0 for to penalize not getting anything done a little more

--a class for functions that predict the long term value associated with a state-action pair
class QFunc f where
  estimate :: f -> WorldView -> NextActions -> Float--estimates values, not next reward
  update :: f -> WorldView -> NextActions -> WorldView -> Float -> f

