import Data.Char (isNumber)
data Race = Race { time :: Int, distance :: Int } deriving Show

parseRaces :: [String] -> [Race]
parseRaces lines = races where
  times = parseNumbers (head lines)
  dists = parseNumbers (last lines)
  races = zipWith Race times dists

parseNumbers :: String -> [Int]
parseNumbers [] = []
parseNumbers str = num : parseNumbers remainingStr where
  strippedStr = dropWhile (not . isNumber) str
  numStr = takeWhile isNumber strippedStr
  remainingStr = drop (length numStr) strippedStr
  num = read numStr

countWaysToWin :: Race -> Int
countWaysToWin race = numWins where
  tr = time race
  dr = distance race
  speeds = [0..tr]
  remainingTimes = reverse speeds
  dists = map (\(s,t) -> s * t) (zip speeds remainingTimes)
  numWins = length $ filter (> dr) dists

main :: IO ()
main = do
  contents <- getContents
  let inputLines = lines contents
  let races = parseRaces inputLines
  print races
  let numWaysToWin = map countWaysToWin races
  print $ product numWaysToWin

lstrip :: String -> String
lstrip = dropWhile (==' ')