import Prelude hiding (lookup)
import Data.List.Split (splitOn)

countArrangements :: String -> Int 
countArrangements str = length possibleArrangements where 
  [springs, damGroups] = splitOn " " str
  -- damaged group arrangements
  dgas = map read (splitOn "," damGroups) :: [Int]
  allArrangements = enumerateArrangements springs
  possibleArrangements = filter (isPossible dgas) allArrangements

enumerateArrangements :: String -> [String] 
enumerateArrangements [] = [[]]
enumerateArrangements ('.':sprs) = map ('.':) (enumerateArrangements sprs)
enumerateArrangements ('#':sprs) = map ('#':) (enumerateArrangements sprs)
enumerateArrangements ('?':sprs) = map ('.':) (enumerateArrangements sprs)
  ++ map ('#':) (enumerateArrangements sprs)

isPossible :: [Int] -> String -> Bool
isPossible dgas springs = springDgas == dgas where 
  springDgas = damagedArrangementInSprings springs

damagedArrangementInSprings :: String -> [Int]
damagedArrangementInSprings [] = []
damagedArrangementInSprings str = if length damaged > 0
  then length damaged : damagedArrangementInSprings remaining
  else [] where 
  temp = dropWhile (/= '#') str
  damaged = takeWhile (== '#') temp
  remaining = dropWhile (== '#') temp

main :: IO ()
main = do
  contents <- getContents
  let inputLines = lines contents
  let possibleArrangementCounts = map countArrangements inputLines
  print $ sum possibleArrangementCounts
