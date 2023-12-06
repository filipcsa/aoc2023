-- There are much more efficient ways to make this work
-- 1. It can probably be shown that the problem is symmetric
--    So it should be enough to get the lower bound on (time pressed) win to deduce
--    The value of the higher bound. The wins always appear in a single block.
-- 2. Not sure about this but it might be possible to get the bound analytically ...

import Data.Char (isNumber)
data Race = Race { time :: Int, distance :: Int } deriving Show

parseRace :: [String] -> Race
parseRace lines = Race time dist where
  time = parseNumber (head lines)
  dist = parseNumber (last lines)

parseNumber :: String -> Int
parseNumber str = read $ filter isNumber str

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
  let race = parseRace inputLines
  print race
  print $ countWaysToWin race

lstrip :: String -> String
lstrip = dropWhile (==' ')