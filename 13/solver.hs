import Prelude hiding (lookup)
import Data.List.Split (splitOn)

type Pattern = [String]
data Reflection = Horizontal Int | Vertical Int deriving Show

findReflection :: Pattern -> Reflection
findReflection pattern = case tryFindHorizontal pattern of
  [n] -> Horizontal n
  [] -> Vertical $ findVertical pattern

tryFindHorizontal :: Pattern -> [Int]
tryFindHorizontal pattern = filter (verifyHorizontal pattern) horizontalCandidates where 
  horizontalPairs = zip pattern (tail pattern)
  pairsWithIdx = zip horizontalPairs [1..]
  horizontalCandidates = 
    map snd $
    filter (\((r1, r2), idx) -> r1 == r2) pairsWithIdx

verifyHorizontal :: Pattern -> Int -> Bool
verifyHorizontal pattern n = and [ l == r | (l,r) <- zip revLeft right ] where 
  revLeft = reverse $ take n pattern
  right = drop n pattern

findVertical :: Pattern -> Int
findVertical pattern = head $ filter (verifyVertical pattern) verticalCandidates where
  verticalCandidates = [1..(length $ head pattern)]

verifyVertical :: Pattern -> Int -> Bool
verifyVertical pattern n = and [ verifyVerticalLine line n | line <- pattern ]

verifyVerticalLine :: String -> Int -> Bool
verifyVerticalLine line n = and [ l == r | (l,r) <- zip revLeft right ] where 
  revLeft = reverse $ take n line
  right = drop n line

summarizeReflections :: Int -> Reflection -> Int
summarizeReflections acc (Vertical n) = acc + n
summarizeReflections acc (Horizontal n) = acc + 100*n

main :: IO ()
main = do
  contents <- getContents
  let inputLines = lines contents
  let patterns = splitOn [""] inputLines
  let reflections = map findReflection patterns
  print reflections
  print $ foldl summarizeReflections 0 reflections