module Main where

--import System.IO
import qualified Data.Csv as Csv
import qualified Data.ByteString.Lazy as BS
import qualified Data.Vector as Vec
import qualified Data.Binary as Bin

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
1. Get some sort of file IO and CSV working so I know how to use these
2. Get a datatype for the contents of a single CSV row
3. make an instance of ToRecord for the above by applying toField to each entry, storing results in a list, and then calling record on that list.
4.  

-}

test = "."


sd = SampleData 1 2
--sd = AttractorLogRow 1 2 1 2 1 2 1 2 1 2 1 2 1 2 1 2 1 2 1 2 1 2 1
header = Vec.fromList $ fmap (BS.toStrict . Bin.encode) ["bet", "alp"]
--header = Vec.fromList $ fmap (BS.toStrict . Bin.encode) LogScenario.namesRow

main :: IO ()
main = BS.writeFile "./sampleCSV" $ Csv.encodeByName header [sd]
-- $ Csv.encode [namesRow]