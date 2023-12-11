import Prelude hiding (lookup)
import Data.Map ( Map, insert, empty, lookup, keys )
import Data.Maybe (fromJust)

type LRSucc = (String, String)
type SuccMap = Map String LRSucc
type Move = Char

parseInput :: [String] -> ([Move], SuccMap)
parseInput lines = (moves, succMap) where
  moves = head lines
  succMap = foldl parseSuccMap empty (drop 2 lines)

parseSuccMap :: SuccMap -> String -> SuccMap
parseSuccMap succMap str = newSuccMap where
  key = take 3 str
  left = take 3 $ drop 7 str
  right = take 3 $ drop 12 str
  newSuccMap = insert key (left, right) succMap

countSteps :: [Move] -> SuccMap -> Int
countSteps moves succMap = numMoves where
  starts = collectStarts succMap
  findSucc = getMoveSucc succMap
  findMove = getMove moves
  numMoves = performMove starts findMove findSucc 0

collectStarts :: SuccMap -> [String]
collectStarts succMap = filter (\str -> last str == 'A') (keys succMap)

performMove :: [String] -> (Int -> Move) -> (Move -> String -> String) -> Int -> Int
performMove currs findMove findSucc step
  | isEnding currs = step
  | otherwise = performMove nexts findMove findSucc (step + 1)
  where
    move = findMove step
    nexts = map (findSucc move) currs

isEnding :: [String] -> Bool
isEnding currs = and [ last curr == 'Z' | curr <- currs ]

getMoveSuccs :: [String] -> Move -> SuccMap -> [String]
getMoveSuccs currs move succMap = succs where
  lrSuccs = [ fromJust $ lookup curr succMap | curr <- currs ]
  succ lr = case move of
    'L' -> fst lr
    'R' -> snd lr
  succs = map succ lrSuccs

getMoveSucc :: SuccMap -> Move -> String -> String
getMoveSucc succMap move curr = succ where
  lrSucc = fromJust $ lookup curr succMap
  succ = case move of
    'L' -> fst lrSucc
    'R' -> snd lrSucc

getMove :: [Move] -> Int -> Move
getMove moves step = moves !! (step `mod` length moves)

main :: IO ()
main = do
  contents <- getContents
  let inputLines = lines contents
  let (moves, succMap) = parseInput inputLines
  print moves
  print succMap
  print $ countSteps moves succMap