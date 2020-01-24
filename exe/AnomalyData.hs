module Main where

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
-}

test = "."



main :: IO ()
main = putStrLn test