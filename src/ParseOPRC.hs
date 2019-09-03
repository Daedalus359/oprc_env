module ParseOPRC where

--datatypes to be parsed
import Drone
import Env
import WorldState

--parser tools
import Text.Trifecta

import Control.Applicative

--subservient to some of the data structures being parsed
import qualified Data.Map.Strict as M
import Data.Maybe

resultToMaybe :: Result a -> Maybe a
resultToMaybe (Failure _) = Nothing
resultToMaybe (Success a) = Just a

--parses an integer, packs that int into a Drone
parseDrone :: Parser Drone
parseDrone = do
  id <- fmap fromInteger natural --natural parses a positive integer
  return (DroneID id)

parseCardinal :: Parser CardinalDir
parseCardinal = do
  dirStr <- some letter
  case dirStr of
    "North" -> return North
    "South" -> return South
    "East" -> return East
    "West" -> return West
    _ -> fail "Could not parse direction"

parseIntercardinal :: Parser IntercardinalDir
parseIntercardinal = do
  dirStr <- some letter
  case dirStr of
    "NE" -> return NE
    "SE" -> return SE
    "NW" -> return NW
    "SW" -> return SW
    _ -> fail "Could not parse direction"

--parser for vertical directions
parseVertical :: Parser VerticalDirection
parseVertical = do
  vertString <- some letter
  case vertString of
    "Ascend" -> return Ascend
    "Descend" -> return Descend

parseAction :: Parser Action
parseAction = do
  actionStr <- some letter
  _ <- spaces
  case actionStr of
    "MoveCardinal" -> do
      dir <- parseCardinal
      return (MoveCardinal dir)
    "MoveIntercardinal" -> do
      dir <- parseIntercardinal
      return (MoveIntercardinal dir)
    "MoveVertical" -> do
      dir <- parseVertical
      return (MoveVertical dir)
    "Hover" -> return Hover
    _ -> fail "Could not parse action"

--parses something inside of two delimeters such as '(' and ')'
parseBetween :: Char -> Char -> Parser a -> Parser a
parseBetween beginC endC p =
  char beginC *> p <* char endC

parseParens :: Parser a -> Parser a
parseParens = parseBetween '(' ')'

parseBrackets :: Parser a -> Parser a
parseBrackets = parseBetween '[' ']'

parseNextActions :: Parser NextActions
parseNextActions = parseBrackets $ (spaces *> singleNA) `sepBy` (symbol ",")

parseEnvironment :: Parser Environment
parseEnvironment = fmap (Environment . M.fromList) $ recursiveParseEnv 0

recursiveParseEnv :: Int -> Parser [(Position, Patch)]
recursiveParseEnv i = (eof *> (return [])) <|> (liftA2 (++)  ((lineToEntries i) <* (optional newline)) (recursiveParseEnv (i + 1))) -- (try eof) <|>

toDetailReq :: Parser (Maybe DetailReq)
toDetailReq =  do
  c <- anyChar
  case c of 
    'H' -> return $ Just Close
    'L' -> return $ Just Far
    ' ' -> return Nothing
    _ -> fail "Environment file must only contain the character 'H', 'L', and ' ' (space)."

toDetailReqs :: Parser [Maybe DetailReq]
toDetailReqs = many $ try toDetailReq

vertBundle :: Int -> Int -> (Maybe DetailReq) -> (Position, Maybe Patch)
vertBundle yc xc mdr = (Position xc yc, fmap Patch mdr)

lineToEntries :: Int -> Parser [(Position, Patch)]
--Integer is the YCoord for all of the values in this string, index in string is XCoord
lineToEntries i = fmap catMaybes $ (fmap . fmap) sequence $ fmap (zipWith (vertBundle i) [0..]) toDetailReqs

--parses one element of a NextActions list, e.g. "(2, MoveVertical Ascend)"
singleNA :: Parser (Drone, Action)
singleNA = parseParens $ do
  drone <- parseDrone
  _ <-  char ','
  _ <- spaces
  action <- parseAction
  return (drone, action)

--shows how the parsers work
parseDemo :: IO ()
parseDemo = do
  let p parser string = print $ parseString parser mempty string
  p parseDrone "1"
  p parseCardinal "North"
  p parseCardinal "Blech"
  p parseIntercardinal "NE"
  p parseIntercardinal "Blech"
  p parseAction "MoveCardinal East"
  p parseAction "MoveIntercardinal SW"
  p parseAction "MoveVertical Ascend"
  p parseAction "Hover"
  p (parseParens letter) "(a)"
  p singleNA "(2, MoveVertical Ascend)"
  p parseNextActions "[(1, Hover), (2, MoveVertical Ascend)]"
  p parseNextActions "[]"

parseEnvDemo :: IO (Result Environment)
parseEnvDemo = fmap (parseString parseEnvironment mempty) $ readFile "./test/environments/2.env"