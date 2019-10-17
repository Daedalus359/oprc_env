module PrettyOPRC where

import Data.Text.Prettyprint.Doc
import WorldState
import Env
import EnvView
import Ensemble
import GraphOPRC
import Drone
import Scenario
import ShapeSweepAgent
import SpanningTreeAgent

import qualified Data.Map.Strict as M
import qualified Data.Set as Set

instance Pretty Position where
  pretty pos = pretty $ show pos

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

instance Pretty (Scenario p) where
  pretty (Scenario policy ws time hist) =
    nest 2 $ vsep [pretty "Scenario: ", pretty ws, pretty "Time: " <+> pretty time, vsep $ fmap pretty hist]

instance Pretty ScenarioReplay where
  pretty (ScenarioReplay ws i moveHist) =
    nest 2 $ vsep [pretty "ScenarioReplay: ", pretty ws, pretty i, vsep $ fmap pretty moveHist]

instance Pretty DroneTerritory where
  pretty (DroneTerritory drone mean) =
    nest 2 $ vsep [pretty "DroneTerritory: ", pretty drone, pretty $ show mean]

instance Pretty DTDirs where
  pretty (DTDirs (DroneTerritory drone mean) dirs) =
    nest 2 $ vsep [pretty "DTDirs: ", pretty drone, pretty $ show mean, nest 2 $ vsep $ (:) (pretty "Directions: ") $  fmap pretty dirs]

--covers Footprint
instance Pretty a => Pretty (Set.Set a) where
  pretty set = vsep $ fmap pretty $ Set.toList set

instance (Pretty k, Pretty v) => Pretty (M.Map k v) where
  pretty map = vsep $ fmap prettyAssignment $ M.toList map
    where
      prettyAssignment (k, v) = nest 2 $ vsep [pretty "assignment: ", pretty k, pretty v]

instance Pretty KMeansLowPolicy where
  pretty (KMeansLowPolicy gen map) =
    nest 2 $ vsep [pretty "KMeansLowPolicy: ", pretty "StdGen:" <+> (pretty $ show gen), nest 2 $ vsep [pretty "territory map: ", pretty map]]

instance Pretty LowKMeansSpanningTreePolicy where
  pretty (LowKMeansSpanningTreePolicy gen map) =
    nest 2 $ vsep [pretty "KMeansLowSpanningTreePolicy: ", pretty "StdGen:" <+> (pretty $ show gen), nest 2 $ vsep [pretty "territory map: ", pretty map]]