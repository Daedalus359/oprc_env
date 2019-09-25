module Main where

import Env
import EnvView
import ParseOPRC
import Policy
import Scenario
import ShapeSweepAgent

import Control.Monad
import Data.Maybe
import System.Directory
import System.Environment
import System.Random
import Text.Trifecta

main :: IO ()
main = do
  kmp <- initializeKMP 10 <$> newStdGen --eventually make this change with configuration
  args <- getArgs
  case args of
    [] -> putStrLn "No argument provided. Re-run with a directory containing only environment files"
    (filePath:args) -> readFilesAt filePath >>= (runScenarios timeLimit numDrones kmp) >> (return ())

  where
    timeLimit = 100000
    numDrones = 3

readFilesAt :: String -> IO [Environment]
readFilesAt dirPath = fmap allEnvs (allFiles >>= readFiles)
  where
    allFiles :: IO [FilePath]
    allFiles = fmap (fmap ((++) dirPath)) $ listDirectory dirPath

    readFiles :: [FilePath] -> IO [String]
    readFiles paths = sequenceA $ fmap readFile paths

    allEnvs :: [String] -> [Environment]
    allEnvs fileStrings = catMaybes $ fmap toMaybe $ fmap (parseString parseEnvironment mempty) fileStrings

    toMaybe :: Result a -> Maybe a
    toMaybe (Success a) = Just a
    toMaybe (Failure _) = Nothing

runScenarios :: (PersistentPolicy p) => Integer -> Int -> (WorldView -> p) -> [Environment] -> IO ([ScenarioReplay], (WorldView -> p))
runScenarios timeLimit numDrones policyF envs = foldr (runPolicyAccum timeLimit numDrones) (return ([], policyF)) envs

--foldr function
runPolicyAccum :: (PersistentPolicy p) => Integer -> Int -> Environment -> IO ([ScenarioReplay], (WorldView -> p)) -> IO ([ScenarioReplay], (WorldView -> p))
runPolicyAccum timeLimit numDrones env ioVal = do
  (replayList, policyF) <- ioVal
  (finished, scenario) <- return $ Scenario.fullRun timeLimit numDrones policyF env
  putStrLn $ "Scenario finished: " ++ (show finished)
  if finished
    then putStrLn $ "Total time: " ++ (show $ getTime scenario)
    else putStrLn ""
  return (createReplay scenario : replayList, cleanup $ getPolicy scenario)
      
printList :: [Environment] -> IO ()
printList strings = foldr (\env -> \soFar -> soFar >> (putStrLn $ take 50 $ show env)) (putStrLn "all files:") strings