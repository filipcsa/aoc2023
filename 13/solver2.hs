import Prelude hiding (lookup)
import Data.List.Split (splitOn)
import Data.Maybe (maybeToList, catMaybes, isJust, fromJust)

type Pattern = [String]
data Reflection = Horizontal Int | Vertical Int deriving (Show, Eq)

tryFindReflection :: Pattern -> Maybe Reflection
tryFindReflection pattern = case tryFindHorizontal pattern of
  [n] -> Just $ Horizontal n
  _ -> case findVertical pattern of
    [n] -> Just $ Vertical n
    _  -> Nothing

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

findVertical :: Pattern -> [Int]
findVertical pattern = filter (verifyVertical pattern) verticalCandidates where
  verticalCandidates = [1..length (head pattern)-2]

verifyVertical :: Pattern -> Int -> Bool
verifyVertical pattern n = and [ verifyVerticalLine line n | line <- pattern ]

verifyVerticalLine :: String -> Int -> Bool
verifyVerticalLine line n = and [ l == r | (l,r) <- zip revLeft right ] where
  revLeft = reverse $ take n line
  right = drop n line

summarizeReflections :: Int -> Reflection -> Int
summarizeReflections acc (Vertical n) = acc + n
summarizeReflections acc (Horizontal n) = acc + 100*n

trySmudgeOptions :: Pattern -> [Pattern]
trySmudgeOptions pattern =
  [ trySmudgeAt pattern i j |
  i <- [0..length pattern - 1], j <- [0..length (head pattern) - 1] ]

trySmudgeAt :: Pattern -> Int -> Int -> Pattern
trySmudgeAt pattern i j =
  [ [ if x == i && y == j then clearSmudge (pattern !! x !! y) else pattern !! x !! y
      | y <- [0..length (head pattern) - 1] ]
      | x <- [0..length pattern - 1] ]

clearSmudge :: Char -> Char
clearSmudge '.' = '#'
clearSmudge '#' = '.'

debugPrint :: (Pattern, Maybe Reflection) -> IO ()
debugPrint (pattern, mr) = do
  print mr
  sequence_ [print pr | pr <- pattern]
  print ""

onlyNewOnes :: (Maybe Reflection, [Maybe Reflection]) -> Reflection
onlyNewOnes (origRef, newRefls) = newRefl where
  trotl = filter (\mr -> isJust mr && mr /= origRef) newRefls
  newRefl = if not (null trotl)
    then fromJust $ head trotl
    else fromJust $ head $ filter (\mr -> isJust mr) newRefls


main :: IO ()
main = do
  contents <- getContents
  let inputLines = lines contents
  let patterns = splitOn [""] inputLines :: [Pattern]
  let origReflections = map tryFindReflection patterns
  let allPatterns = map trySmudgeOptions patterns
  let maybeReflections = map (map tryFindReflection) allPatterns
  let newReflections = zipWith (curry onlyNewOnes) origReflections maybeReflections
  -- print 0
  print $ foldl summarizeReflections 0 newReflections