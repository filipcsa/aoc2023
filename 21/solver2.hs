import Text.Regex.TDFA ( (=~), AllMatches(getAllMatches), AllTextMatches (getAllTextMatches) )
import Prelude hiding (lookup)
import Data.List (nub, nubBy)
import Data.Set (fromList, Set, member)

type Layout = [[Char]]
type Pos = (Int, Int)

data Path =
    Source { pos :: Pos }
  | Node { pos :: Pos, prev :: Path }

-- OBSERVATIONS
-- if a pos is reachable in n steps, it is then also reachable in n+2 steps (go to prev and back)

-- Lets first compute n0 for all the poss on a single (non infinite) map. 
-- We can the partiotion the poss into two sets
  -- poss reachable when n is odd (if n0 of poss is odd)
  -- poss reachable when n is even (if n0 of poss is even)

-- The question then remains how to compute n0 for far away poss,
-- Clearly, the fact that the map is repeated can be used somehow.

findStartingPos :: Layout -> Pos
findStartingPos layout =
  head $ [ (x,y) | x <- [0..h-1], y <- [0..w-1], layout !! x !! y == 'S' ] where
    h = length layout
    w = length $ head layout

getReachablePossInSteps :: Layout -> Pos -> Int -> [Pos]
getReachablePossInSteps layout start steps = reachablePoss where
  paths = [ Source start ]
  reachablePathsOfLength = bfsNodesInDepth layout paths steps
  reachablePoss = map pos reachablePathsOfLength

bfsNodesInDepth :: Layout -> [Path] -> Int -> [Path]
bfsNodesInDepth _ paths 0 = paths
bfsNodesInDepth layout paths n =
  bfsNodesInDepth layout updatedPaths (n-1) where
    extendedPaths = concatMap (extendPath layout) paths
    updatedPaths = nubBy (\a b -> pos a == pos b) extendedPaths

extendPath :: Layout -> Path -> [Path]
extendPath layout path = extendedPaths where
  possInPathSoFar = fromList $ getPathPoss path
  nextCandidates = neighbors $ pos path
  validNextPoss = filter (validNext layout) nextCandidates
  extendedPaths = [ Node pos path | pos <- validNextPoss ]

getPathPoss :: Path -> [Pos]
getPathPoss (Source pos) = [pos]
getPathPoss (Node pos prev) = pos : getPathPoss prev

neighbors :: Pos -> [Pos]
neighbors (x,y) = [ (x-1, y), (x+1, y), (x, y-1), (x, y+1) ]

validNext :: Layout -> Pos -> Bool
validNext layout (x,y) = isInBounds && isPlotTile where
  h = length layout
  w = length $ head layout
  isInBounds = x >= 0 && y >= 0 && x < h && y < w
  isPlotTile = (layout !! x !! y) `elem` ['.', 'S']

-- Printig for debug
showReachable :: Layout -> [Pos] -> IO ()
showReachable layout poss =
  sequence_ [ showReachableInRow layout poss r | r <- [0..length layout - 1] ]

showReachableInRow :: Layout -> [Pos] -> Int -> IO ()
showReachableInRow layout poss r =
  print [ chooseChar layout poss r c | c <- [0..length (head layout) - 1] ]

chooseChar :: Layout -> [Pos] -> Int -> Int -> Char
chooseChar layout poss r c = char where
  isVisited = (r,c) `elem` poss
  char = if isVisited
    then 'O'
    else layout !! r !! c


main :: IO ()
main = do
  contents <- getContents
  let layout = lines contents :: Layout
  let startingPos = findStartingPos layout
  print startingPos
  let reachablePossInSteps = getReachablePossInSteps layout startingPos 64
  -- showReachable layout reachablePossInSteps
  print $ length reachablePossInSteps