import Prelude hiding (lookup)
import Data.Map ( Map, insert, empty, lookup )
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
  infMoves = cycle moves
  numMoves = performMove infMoves "AAA" succMap

performMove :: [Move] -> String -> SuccMap -> Int 
performMove _ "ZZZ" _ = 0
performMove (move:moves) curr succMap = 1 + performMove moves next succMap where 
  next = getMoveSucc curr move succMap

getMoveSucc :: String -> Move -> SuccMap -> String 
getMoveSucc key move succMap = succ where 
  lrSuccs = fromJust $ lookup key succMap
  succ = case move of 
    'L' -> fst lrSuccs
    'R' -> snd lrSuccs

main :: IO ()
main = do
  contents <- getContents
  let inputLines = lines contents
  let (moves, succMap) = parseInput inputLines
  print moves
  print succMap
  print $ countSteps moves succMap