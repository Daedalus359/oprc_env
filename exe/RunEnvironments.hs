module Main where

import Env

import System.Directory
import System.Environment

main :: IO ()
main = do
  args <- getArgs
  case args of
    [] -> putStrLn "No argument provided. Re-run with a directory containing only environment files"
    (filePath:args) -> runThroughAt filePath

runThroughAt :: String -> IO ()
runThroughAt dirPath = allFiles >>= printList
  where
    allFiles :: IO [String]
    allFiles = listDirectory dirPath

    printList :: [String] -> IO ()
    printList strings = foldr (\str -> \soFar -> soFar >> (putStrLn str)) (putStrLn "all files:") strings