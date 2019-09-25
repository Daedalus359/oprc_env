module Main where

import Env
import ParseOPRC

import Control.Monad
import Data.Maybe
import System.Directory
import System.Environment
import Text.Trifecta

main :: IO ()
main = do
  args <- getArgs
  case args of
    [] -> putStrLn "No argument provided. Re-run with a directory containing only environment files"
    (filePath:args) -> readFilesAt filePath >> (return ())

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

runScenarios :: [Environment] -> IO ()
runScenarios envs = undefined --foldr undefined undefined undefined

printList :: [Environment] -> IO ()
printList strings = foldr (\env -> \soFar -> soFar >> (putStrLn $ take 50 $ show env)) (putStrLn "all files:") strings