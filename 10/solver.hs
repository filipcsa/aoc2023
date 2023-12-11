import Prelude hiding (lookup)
import Data.Set (Set, singleton, empty, insert, member, fromList)

type Pos = (Int, Int)
type Layout = [[Char]]
type Visited = Set Pos

findStart :: Layout -> Pos
findStart layout = head [ (x, y) | x <- [0..xx], y <- [0..yy],
 (layout !! x) !! y == 'S' ] where
  xx = length layout - 1
  yy = length (head layout) - 1

findCycle :: Layout -> Pos -> [Pos]
findCycle layout = dfsCycle layout empty

dfsCycle :: Layout -> Visited -> Pos -> [Pos]
dfsCycle layout visited pos
  | member pos visited && length visited > 2 && getTile layout pos == 'S' = [pos]
  | member pos visited = []
  | otherwise = cycle where
  feasibleNeighbors = findNeighbors layout pos
  newVisited = insert pos visited
  succPaths = map (dfsCycle layout newVisited) feasibleNeighbors
  validPaths = filter (/=[]) succPaths
  cycle = case validPaths of
    [] -> []
    some -> pos : head some

getTile :: Layout -> Pos -> Char
getTile layout (x, y) = (layout !! x) !! y

findNeighbors :: Layout -> Pos -> [Pos]
findNeighbors layout pos = neighbors where
  tile = getTile layout pos
  neighbors = filter (validPos layout) (validNeighbors pos tile)

validPos :: Layout -> Pos -> Bool
validPos layout (x, y) = x >= 0 && x < lx && y >= 0 && y < ly where
  lx = length layout
  ly = length $ head layout

validNeighbors :: Pos -> Char -> [Pos]
validNeighbors (x, y) '|' = [(x-1, y), (x+1, y)]
validNeighbors (x, y) '-' = [(x, y-1), (x, y+1)]
validNeighbors (x, y) 'L' = [(x-1, y), (x, y+1)]
validNeighbors (x, y) 'J' = [(x-1, y), (x, y-1)]
validNeighbors (x, y) '7' = [(x+1, y), (x, y-1)]
validNeighbors (x, y) 'F' = [(x+1, y), (x, y+1)]
validNeighbors (x, y) '.' = []
validNeighbors (x, y) 'S' = [(x-1, y), (x+1, y), (x, y-1), (x, y+1)]

main :: IO ()
main = do
  contents <- getContents
  let layout = lines contents
  let startPos = findStart layout
  print startPos
  let cycle = findCycle layout startPos
  print cycle
  print $ length cycle `div` 2