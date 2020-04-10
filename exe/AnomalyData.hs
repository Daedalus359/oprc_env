module Main where

--import System.IO
import Control.Monad
import qualified Data.Csv as Csv
import qualified Data.ByteString.Lazy as BS
import qualified Data.Vector as Vec
import qualified Data.Binary as Bin
import qualified SampleVals as SV
import Env
import EnvView
import HierarchicalPolicy
import AnomalousPolicy

import LogScenario

{-
REQUIREMENTS
------------
Final data ex[prted as CSV
All drone data in 1 file vs seperate file for each drone - try both eventually
Same # of drones in every run (4?)
Fields:
  Position of Each Drone (Interpolated, See Below)
  Distance between each drone and every other drone
  Median Distance Between Drones
  Ratio of the above two
  How Fast Drones Are Approaching Each Other - Average of maybe 5 time steps?
  # of in-bounds positions below drone
  ratio of blue vs green in bounds positions below drone
Drone Position Changes: Drone moving from (1, 1) to (2, 2) with seven time steps remaining is now at (1.5, 1.5) and moving NE
  Use Cost function for each move type, divide (cost - remaining) by cost and truncate to two decimal places.
-}

nominalEnvs :: IO [Environment]
nominalEnvs = sequenceA $ fmap SV.envFromFilePath $ fmap (\ns -> "./test/environments/generated/attractor_nominal_environments/testMixed" ++ ns ++ ".env") $ fmap show [1 .. 100]

attractorPolicy :: Bool -> IO (WorldView -> HighFirstBFSPolicyAn)
attractorPolicy True = undefined --SV.hfsp
attractorPolicy False = SV.anomp

ioSlogs :: Bool -> IO [ScenarioLog]
ioSlogs True = fmap fmap (fullLogRun 15000 4 <$> (attractorPolicy True)) <*> nominalEnvs
ioSlogs False = join $ fmap sequenceA $ fmap fmap ioLogger <*> nominalEnvs
  where
    ioLogger :: IO (Environment -> IO ScenarioLog)
    ioLogger = (fullLogRunIO 15000 4 <$> (attractorPolicy False))

ioCSVs :: Bool -> IO [BS.ByteString]
ioCSVs nominal = fmap ((fmap $ Csv.encodeByName header) . (fmap mkAttractorData)) (ioSlogs nominal)

makeFiles :: Bool -> [BS.ByteString] -> IO ()
makeFiles nominal logs = foldr (>>) (return ()) actions 
  where
    actions :: [IO ()]  
    actions = zipWith (\fn -> \log -> putStrLn ("Generating file: " ++ fn) >> BS.writeFile fn log) fileNames logs

    fileNames :: [String]
    fileNames = fmap (("./attractorData/" ++ nominalityStr ++ "/data_" ++ nominalityStr ++ "_") ++ ) $ fmap show [1 ..]

    nominalityStr = if nominal then "nominal" else "anomalous"

header = Vec.fromList $ fmap (BS.toStrict . Bin.encode) LogScenario.namesRow

--

main :: IO ()
main = (ioCSVs nominal) >>= (makeFiles nominal)
  where
    nominal = False --generate nominal or anomalous data