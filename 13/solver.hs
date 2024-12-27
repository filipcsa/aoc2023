import Data.List.Split (splitOn)
import Data.List (transpose)

main :: IO ()
main = do
  contents <- getContents
  let patterns = splitOn [""] $ lines contents
  let mirrorHV = map findReflectionHV patterns
  print $ sum $ map (\(r,c) -> 100*r + c) mirrorHV

findReflectionHV :: [String] -> (Int, Int)
findReflectionHV strs = (findReflection strs, findReflection (transpose strs))

findReflection :: [String] -> Int
findReflection strs = if null reflections then 0 else head reflections
  where reflections = filter (isReflectingIdx strs) [1..length strs - 1]

isReflectingIdx :: [String] -> Int -> Bool
isReflectingIdx strs idx = all (uncurry (==)) $ zip (reverse $ take idx strs) (drop idx strs)
