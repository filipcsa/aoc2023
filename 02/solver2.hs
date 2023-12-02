import Data.List.Split (splitOn, splitOneOf)

type RGB = (Int, Int, Int)
data Game = Game { gameId :: Int, maxRGB :: RGB} deriving Show
data Color = R | G | B

parseGame :: String -> Game
parseGame str = Game { gameId=gId, maxRGB=rgb } where
  gId = parseGameId $ takeWhile (/=':') str
  colorStrs = splitOneOf ",;" $ tail $ dropWhile (/=':') str
  rgb = foldl processColorStrs (0, 0, 0) colorStrs

parseGameId :: String -> Int
parseGameId str = read $ splitOn " " str !! 1

processColorStrs :: RGB -> String  -> RGB
processColorStrs (r,g,b) str = updatedRGB where
  [numStr, colorStr] = splitOn " " $ lstrip str
  num = read numStr :: Int
  updatedRGB = case colorStr of
    "red"   -> (max num r, g, b)
    "green" -> (r, max num g, b)
    "blue"  -> (r, g, max num b)

isGamePossible :: Game -> Bool
isGamePossible game = isRGBPossible $ maxRGB game

isRGBPossible :: RGB -> Bool
isRGBPossible (r, g, b) = r <= 12 && g <= 13 && b <= 14

getSetPower :: Game -> Int
getSetPower game = r*g*b where
  (r, g, b) = maxRGB game

main :: IO ()
main = do
  contents <- getContents
  let inputLines = lines contents
  let games = map parseGame inputLines
  let setPowers = map getSetPower games
  print $ sum setPowers

lstrip :: String -> String
lstrip = dropWhile (==' ')