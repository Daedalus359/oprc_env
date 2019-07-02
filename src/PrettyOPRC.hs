module PrettyOPRC where

import Data.Text.Prettyprint.Doc
import WorldState
import Env
import EnvView
import Ensemble
import Drone

import qualified Data.Map as M

testDecl = "."

--instance Pretty WorldState where
  --pretty ws =

instance Pretty Drone where
  pretty (DroneID n) = pretty "Drone #" <> pretty n

instance Pretty DronePosition where
  pretty (DronePos pos alt) = (pretty $ show alt) <> pretty " over " <> (pretty $ show pos)

instance Pretty IntercardinalDir where
  pretty NE = pretty "North-East"
  pretty SE = pretty "South-East"
  pretty NW = pretty "North-West"
  pretty SW = pretty "South-West"

instance Pretty Action where
  pretty (MoveCardinal cd) = pretty "Move " <> (pretty $ show cd)
  pretty (MoveIntercardinal dir) = pretty "Move " <> (pretty dir)
  pretty (MoveVertical vdir) = pretty $ show vdir
  pretty Hover = pretty "Hover"

instance Pretty DroneStatus where
  pretty (Unassigned dpos) = pretty "Unassigned, " <> (pretty dpos)
  pretty (Assigned action dpos) = pretty dpos <> pretty ", assigned to " <> pretty action
  pretty (Acting action stepsRem dpos) = vsep [pretty dpos,
                                              pretty action <> pretty " in progress with " <> (pretty $ show stepsRem) <> pretty " steps remaining"]

instance Pretty Environment where
  pretty (Environment map) = vsep $ fmap (pretty . show) (M.toList map)