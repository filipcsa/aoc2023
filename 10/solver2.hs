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

findSomeStart :: Layout -> Set Pos -> Pos
findSomeStart layout cycle = head $ filter notInCycle edgePoss where
  notInCycle p = not $ member p cycle
  xx = length layout - 1
  yy = length (head layout) - 1
  edgePoss = [(x,0) | x <- [0..xx]] ++ [(0,y) | y <- [0..yy]]
    ++ [(x, yy) | x <- [0..xx]] ++ [(xx, y) | y <- [0..yy]]

findAllReachable :: Layout -> Pos -> Set Pos -> Set Pos -> Set Pos
findAllReachable layout curr cycle reached
  | member curr reached = reached
  | otherwise = newReached where
    currReached = insert curr reached
    neighbors = reachableNeighbors layout cycle curr
    newReached = foldl (\acc p -> findAllReachable layout p cycle acc) currReached neighbors

reachableNeighbors :: Layout -> Set Pos -> Pos -> [Pos]
reachableNeighbors layout cycle (x,y) = filter (\p -> not $ member p cycle) ns where
 tempns = [(x-1, y), (x+1, y), (x, y-1), (x, y+1)]
 ns = filter (validPos layout) tempns

nonReachablePoss :: Layout -> Set Pos -> Set Pos
nonReachablePoss layout reachable = fromList nonReachable where
  xx = length layout - 1
  yy = length (head layout) - 1
  allPoss = [ (x, y) | x <- [0..xx], y <- [0..yy] ]
  dotPoss = filter (\p -> getTile layout p == '.') allPoss
  nonReachable = filter (\p -> not $ member p reachable) dotPoss

main :: IO ()
main = do
  contents <- getContents
  let layout = lines contents
  let startPos = findStart layout
  print startPos
  let cycle = findCycle layout startPos
  print cycle
  let cycleSet = fromList cycle
  let someStart = findSomeStart layout cycleSet
  print someStart
  let allReachable = findAllReachable layout someStart cycleSet empty
  let nonReachable = nonReachablePoss layout allReachable
  print nonReachable
  print $ length nonReachable