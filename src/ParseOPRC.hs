module ParseOPRC where

--datatypes to be parsed
import Drone
import Env
import WorldState

--parser tools
import Text.Trifecta

--parses an integer, packs that int into a Drone
parseDrone :: Parser Drone
parseDrone = do
  id <- natural --natural parses a positive integer
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