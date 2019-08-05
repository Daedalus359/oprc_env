module PrettyOPRC where

import Data.Text.Prettyprint.Doc
import WorldState
import Env
import EnvView
import Ensemble
import Drone
import Scenario

import qualified Data.Map.Strict as M

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
  pretty (Environment map) = nest 2 $ vsep $ (:) (pretty "Environment: ") $ fmap (pretty . show) (M.toList map)

prettyEnvInfo :: EnvironmentInfo -> Doc ann --not sure what the "ann" means in this - copied from the Docs for prettyprinter
prettyEnvInfo ei = nest 2 $ vsep $ (:) (pretty "EnvInfo") $ fmap (pretty . show) (M.toList ei)--Nest not working as expected!

prettyDroneAndStat :: (Drone, DroneStatus) -> Doc ann
prettyDroneAndStat (drone, status) = nest 2 $ vsep [pretty drone, pretty status]

instance Pretty WorldState where
  pretty ws = nest 2 $ vsep [pretty "WorldState: ", (pretty $ getEnv ws), prettyInfo, prettyEnsemble]
    where prettyInfo = prettyEnvInfo (getInfo ws)
          prettyEnsemble = nest 2 $ vsep $ (:) (pretty "Ensemble Status: ") $ fmap prettyDroneAndStat (getEnsemble ws)

instance Pretty Snapshot where
  pretty (Snapshot commands time) = pretty "Snapshot: " <+> (pretty commands) <+> (pretty "Time: ") <+> (pretty time)