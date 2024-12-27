import Data.List.Split (splitOn)
import Data.List (transpose)

main :: IO ()
main = do
  contents <- getContents
  let patterns = splitOn [""] $ lines contents
  let part1MirrorHV = map (`findReflectionHV` 0) patterns
  let part2MirrorHV = map (`findReflectionHV` 1) patterns
  print $ sum $ map (\(r,c) -> 100*r + c) part1MirrorHV
  print $ sum $ map (\(r,c) -> 100*r + c) part2MirrorHV

findReflectionHV :: [String] -> Int -> (Int, Int)
findReflectionHV strs m = (findReflection strs m, findReflection (transpose strs) m)

findReflection :: [String] -> Int -> Int
findReflection strs m = if null reflections then 0 else head reflections
  where reflections = filter (isReflectingIdx strs m) [1..length strs - 1]

isReflectingIdx :: [String] -> Int -> Int -> Bool
isReflectingIdx strs m idx = numMismatches == m where
  numMismatches = sum
    $ zipWith (\ a b -> length $ filter (uncurry (/=)) $ zip a b) (reverse $ take idx strs) (drop idx strs)
