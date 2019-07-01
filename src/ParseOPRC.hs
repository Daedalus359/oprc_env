module ParseOPRC where

--datatypes to be parsed
import Drone
import Env

--parser tools
import Text.Trifecta

doesItCompile = "?"

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
    --handle movevertical case
    --handle hover case
    _ -> fail "Could not parse action"

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
