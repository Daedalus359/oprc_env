module Main where

--oprc imports
import qualified Env
import MoveCosts
import qualified SampleVals as SV
import WorldState
import Drone
import qualified Scenario
import RandomOPRC
import SerializeOPRC
import EnvGen

--prettyprinting
import PrettyOPRC
import Data.Text.Prettyprint.Doc
import Data.Text.Prettyprint.Doc.Util

--other oprc dependencies
import qualified Data.Map.Strict as Map

--diagrams imports
import qualified Diagrams.Prelude as D
import qualified Diagrams.Backend.SVG.CmdLine as BE

import Control.Monad

--general utilities
import Util (nTimes)
import Control.Applicative
import System.Random as Random

myCircle :: D.Diagram BE.B
myCircle = D.circle 1

demoDatatypes :: IO ()
demoDatatypes = foldr (>>) (return ())
  (fmap putStrLn
    [ "Demonstrating oprc_env on sample data values, see SampleVals.hs for details",
      "\nPretty printing a patch",
      show SV.pat,
      "\nChecking what is at position (1,1): ",
      (show (Map.lookup (Env.Position 1 1) (Env.toMap SV.env))),
      "\nChecking what is at position (5,-3): ",
      (show (Map.lookup (Env.Position 5 (-3)) (Env.toMap SV.env))),
      "\nShowing footprint generated from the environment: ",
      (show SV.footprint),
      "\nprinting neighbors of (3,6): ",
      show $ Env.neighborsOf (Env.Position 3 6),
      "\nlist of locations viewable from (2, 5) at low altitude: ",
      show $ viewableFrom (DronePos (Env.Position 2 5) Env.Low),
      "\nlist of locations viewable from (2, 5) at high altitude: ",
      show $ viewableFrom (DronePos (Env.Position 2 5) Env.High),
      "\nshowing a drone's status",
      show SV.workingUp,
      "\nshowing an ensemble's status",
      show SV.ensembleStatus,
      "\nshowing partial information about an environment",
      show SV.envInfo,
      "\nshowing a WorldState",
      show SV.worldState,
      "\nshowing the same WorldState after 4 updates (no new actions commanded)",
      show $ nTimes 4 (updateState []) SV.worldState,
      ""
    ]
  )

main :: IO ()
main = 
  --SV.manualControl SV.worldState >>
  --SV.dumpParseFailure2 SV.manualControl (SV.liftToWS 1 $ SV.parseEnvNum 4) >> 
  --BE.mainWith myCircle >>
  -- SV.fullScenarioWithOutput SV.randFiltPolicy 4 6 10000 >>
  -- SV.firstStepsWithOutput >> 
  --SV.threeStepsOfOutput SV.lsPolicy 1 2 5 >> 
  --SV.fullScenarioWithOutput (return $ const SV.lsPolicy) 1 7 100000 >>
  --SV.fullScenarioWithOutput SV.randPolicy 1 6 50000 >>
  --putStrLn "Random environment times for environment 6:" >>

  --fmap show (fmap averageRunTime ((SV.dumpParseFailure $ SV.parseEnvNum 7) >>= (randAgentRunTimes 5 1 1000000))) >>= putStrLn >>

  --SV.threeStepsOfOutput SV.kmp 3 9 5 >>

  --SV.fileNameScenarioWithOutput SV.hfsp 3 "./test/environments/generated/clumpedNE=0.25T=0.3.env" 25000 >>

  putStrLn "Average Performance of ..." >>

  --putStrLn "Low Sweep Policy, Simple Environment" >>
  --averageAgentPerformance 2 100 (const SV.lsPolicy) [SV.env] >>= print >>

  --putStrLn "Low Sweep Policy, Simple Square Environment" >>
  --simpleSquareEnv >>= 
    --averageAgentPerformance 4 100000 (const SV.lsPolicy) >>= print >>

  --putStrLn "Clustering Low Policy, Simple Square Environment" >>
    --join ((averageAgentPerformance 4 100000) <$> SV.kmp <*> simpleSquareEnv) >>= print >>

  --putStrLn "Low Spanning Tree Policy, Simple Square Environment" >>
  --join ((averageAgentPerformance 4 100000) <$> SV.lstp  <*> simpleSquareEnv) >>= print >>

  --putStrLn "CLSTP, Simple Square Environment" >>
  --join ((averageAgentPerformance 4 100000) <$> SV.lkmstp  <*> simpleSquareEnv) >>= print >>

  --putStrLn "ALBP, Simple Square Environment" >>
  --join ((averageAgentPerformance 4 100000) <$> SV.albp  <*> simpleSquareEnv) >>= print >>

  --putStrLn "HSTP, Simple Square Environment" >> 
  --join ((averageAgentPerformance 4 100000) <$> SV.hfsp  <*> simpleSquareEnv) >>= print >>

  ----------------
  --80% HS COMPLEX, high80Env
  ----------------

  --putStrLn "Low Sweep Policy, 80% HS Complex Env" >> 
  --high80Env >>= averageAgentPerformance 4 100000 (const SV.lsPolicy) >>= print >>

  --putStrLn "Clustering Low Policy, 80% HS Complex Env" >>
  --join ((averageAgentPerformance 4 100000) <$> SV.kmp <*> high80Env) >>= print >>

  --putStrLn "Low Spanning Tree Policy, 80% HS Complex" >>
  --join ((averageAgentPerformance 4 100000) <$> SV.lstp  <*> high80Env) >>= print >>

  --putStrLn "CLSTP, 80% HS Complex" >>
  --join ((averageAgentPerformance 4 100000) <$> SV.lkmstp  <*> high80Env) >>= print >>

  --putStrLn "ALBP, 80% HS COmplex" >>
  --join ((averageAgentPerformance 4 100000) <$> SV.albp  <*> high80Env) >>= print >>

  --putStrLn "HSTP, 80% HS Complex" >>
  --join ((averageAgentPerformance 4 100000) <$> SV.hfsp  <*> high80Env) >>= print >>

  ----------------
  --20% HS COMPLEX, high20Env
  ----------------

  --putStrLn "Low Sweep Policy, 20% HS Complex Env" >> 
  --high20Env >>= averageAgentPerformance 4 100000 (const SV.lsPolicy) >>= print >>

  --putStrLn "Clustering Low Policy, 20% HS Complex Env" >>
  --join ((averageAgentPerformance 4 100000) <$> SV.kmp <*> high20Env) >>= print >>

  --putStrLn "Low Spanning Tree Policy, 20% HS Complex" >>
  --join ((averageAgentPerformance 4 100000) <$> SV.lstp  <*> high20Env) >>= print >>

  --putStrLn "CLSTP, 20% HS Complex" >>
  --join ((averageAgentPerformance 4 100000) <$> SV.lkmstp  <*> high20Env) >>= print >>

  --putStrLn "ALBP, 20% HS COmplex" >>
  --join ((averageAgentPerformance 4 100000) <$> SV.albp  <*> high20Env) >>= print >>

  --putStrLn "HSTP, 80% HS Complex" >>
  --join ((averageAgentPerformance 4 100000) <$> SV.hfsp  <*> high20Env) >>= print >>

  ----------------
  --20% HS CLUSTERED, high20Clumped
  ----------------

  --putStrLn "Low Sweep Policy, Clumped Environment" >> 
  --high20Clumped >>= averageAgentPerformance 4 100000 (const SV.lsPolicy) >>= print >>

  --putStrLn "Clustering Low Policy, Clumped Environment" >>
  --join ((averageAgentPerformance 4 100000) <$> SV.kmp <*> high20Clumped) >>= print >>

  --putStrLn "Low Spanning Tree Policy, Clumped Environment" >>
  --join ((averageAgentPerformance 4 100000) <$> SV.lstp  <*> high20Clumped) >>= print >>

  --putStrLn "CLSTP, Clumped Environment" >>
  --join ((averageAgentPerformance 4 100000) <$> SV.lkmstp  <*> high20Clumped) >>= print >>

  --putStrLn "ALBP, Clumped Environment" >>
  --join ((averageAgentPerformance 4 100000) <$> SV.albp  <*> high20Clumped) >>= print >>

  --putStrLn "HSTP, Clumped Environment" >>
  --join ((averageAgentPerformance 4 100000) <$> SV.hfsp  <*> high20Clumped) >>= print >>

  ----------------
  --clumpedDropout
  ----------------

  --putStrLn "Low Sweep Policy, Clumped Environment avgAgentPerfDropout" >> 
  --high20Clumped >>= avgAgentPerfDropout 4 100000 (const SV.lsPolicy) >>= print >>

  --putStrLn "Clustering Low Policy, Clumped Environment avgAgentPerfDropout" >>
  --join ((avgAgentPerfDropout 4 100000) <$> SV.kmp <*> high20Clumped) >>= print >>

  putStrLn "Low Spanning Tree Policy, Clumped Environment avgAgentPerfDropout" >>
  join ((avgAgentPerfDropout 4 100000) <$> SV.lstp  <*> high20Clumped) >>= print >>

  --putStrLn "CLSTP, Clumped Environment avgAgentPerfDropout" >>
  --join ((avgAgentPerfDropout 4 100000) <$> SV.lkmstp  <*> high20Clumped) >>= print >>

  putStrLn "ALBP, Clumped Environment avgAgentPerfDropout" >>
  join ((avgAgentPerfDropout 4 100000) <$> SV.albp  <*> high20Clumped) >>= print >>

  putStrLn "HSTP, Clumped Environment avgAgentPerfDropout" >>
  join ((avgAgentPerfDropout 4 100000) <$> SV.hfsp  <*> high20Clumped) >>= print >>

  --encodeEnv <$> (hole <$> newStdGen <*> (newBernoulliEnv <$> newStdGen <*> return 6 <*> return 0 <*> return 25 <*> return 0 <*> return 45 <*> return 0.27) <*> (return $ Env.Position 12 20) <*> return 5) >>= (writeFile "./test/environments/generated/withHole.env") >>
  return ()
  where
    simpleSquareEnv = (sequenceA [SV.envFromFilePath "./test/environments/simpleLargeSquare"])
    high80Env = (sequenceA [SV.envFromFilePath "./test/environments/high80low20complex.env"])
    high20Env = (sequenceA [SV.envFromFilePath "./test/environments/high20low80complex.env"])
    high20Clumped = (sequenceA [SV.envFromFilePath "./test/environments/high20Clumped"])