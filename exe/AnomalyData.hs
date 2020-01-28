module Main where

--import System.IO
import qualified Data.Csv as Csv
import qualified Data.ByteString.Lazy as BS
import qualified Data.Vector as Vec
import qualified Data.Binary as Bin
import qualified SampleVals as SV
import Env

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


STEPS
-----
6. get a list of 100 environments to work likewise
-}

test = "."

easySquareEnv :: IO [Environment]
easySquareEnv = (sequenceA [SV.envFromFilePath "./test/environments/bigEasyGrid.env"])

nominalEnvs :: IO [Environment]
nominalEnvs = sequenceA $ fmap SV.envFromFilePath $ fmap (\ns -> "./test/environments/generated/attractor_nominal_environments/testMixed" ++ ns ++ ".env") $ fmap show [1 .. 2]

eSingle :: IO Environment
eSingle = fmap head easySquareEnv

ioSlog :: IO ScenarioLog
ioSlog = fullLogRun 100000 4 <$> SV.hfsp <*> eSingle

ioRow :: IO [AttractorLogRow]
ioRow = fmap mkAttractorData ioSlog

ioCSV :: IO BS.ByteString
ioCSV = fmap (Csv.encodeByName header) ioRow

ioSlogs :: IO [ScenarioLog]
ioSlogs = fmap fmap (fullLogRun 100000 4 <$> SV.hfsp) <*> nominalEnvs

ioCSVs :: IO [BS.ByteString]
ioCSVs = fmap ((fmap $ Csv.encodeByName header) . (fmap mkAttractorData)) ioSlogs

makeFiles :: [BS.ByteString] -> IO ()
makeFiles logs = foldr (>>) (return ()) actions 
  where
    actions :: [IO ()]
    actions = zipWith BS.writeFile fileNames logs

    fileNames :: [String]
    fileNames = fmap ("./attractorData/nominal/data_nominal_" ++ ) $ fmap show [1 ..]


--


sd = AttractorLogRow 1 2 1 2 1 2 1 2 1 2 1 2 1 2 1 2 1 2 1 2 1 2 1 2

header = Vec.fromList $ fmap (BS.toStrict . Bin.encode) LogScenario.namesRow

--

main :: IO ()
main = ioCSVs >>= makeFiles

 --BS.writeFile "./sampleCSV" $ Csv.encodeByName header [sd]