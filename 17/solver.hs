import qualified Data.PQueue.Prio.Min as Q
import qualified Data.Map as M
import Data.List (nub, minimumBy)

data Dir = N | W | S | E | Z deriving (Eq, Ord, Show)
type Coord = (Int, Int)
type Pos = (Coord, Dir, Int)
type Queue = Q.MinPQueue Int Pos

main :: IO ()
main = do
  contents <- getContents
  let strs = lines contents
  let initPos = ((0,0), Z, 0)
  let q = Q.singleton 0 initPos
  let cs = M.singleton initPos 0
  let (c, prevs, _) = dijkstra strs cs M.empty q 0 4
  let (c2, prevs2, f) = dijkstra strs cs M.empty q 4 11
  print c
  print c2

printPath :: [String] -> [Pos] -> IO ()
printPath strs path = sequence_ [ print [if (x,y) `elem` ps then 'X' else strs !! x !! y | y <- [0..w-1]] | x <- [0..h-1]] where
  ps = map fst3 path
  h = length strs
  w = length $ head strs

retrievePath :: M.Map Pos Pos -> Maybe Pos -> [Pos]
retrievePath prevs Nothing = []
retrievePath prevs (Just n) = retrievePath prevs (M.lookup n prevs) ++ [n]

dijkstra :: [String] -> M.Map Pos Int -> M.Map Pos Pos -> Queue -> Int -> Int -> (Int, M.Map Pos Pos, Pos)
dijkstra strs costs prevs q minDir maxDir
  | fst3 minCoord == (length strs - 1, length (head strs) - 1) && trd3 minCoord > minDir - 2 = (minCost, prevs, minCoord)
  | otherwise = dijkstra strs costs' prevs' q' minDir maxDir where
    (minCost, minCoord) = Q.findMin q
    q0 = Q.deleteMin q
    succs = filter (\(c,p) -> c < M.findWithDefault (10^6) p costs)
      $ map (\((x,y), d, n) -> (minCost + read [strs !! x !! y], ((x,y), d, n)))
      $ neighbors strs minCoord minDir maxDir
    costs' = foldl (\acc (c,p) -> M.insert p c acc) costs succs
    prevs' = foldl (\acc (_,p) -> M.insert p minCoord acc) prevs succs
    q' = foldl (\acc (c,p) -> Q.insert c p acc) q0 succs

fst3 :: Pos -> (Int, Int)
fst3 (c,_,_) = c

trd3 :: Pos -> Int
trd3 (_,_,n) = n

neighbors :: [String] -> Pos -> Int -> Int -> [Pos]
neighbors strs ((x,y), d, n) minDir maxDir =
  filter (\(_,d',n') -> (n > minDir - 2) || (d' == d) || d == Z)
  $ filter (\(_,_,n') -> n' < maxDir - 1)
  $ map (\(coor, d') -> if d' == d then (coor, d', n+1) else (coor, d', 0))
  $ filter (\((x',y'), d') -> x' >= 0 && x' < h && y' >= 0 && y' < w && d' /= opp d)
    [((x-1, y), N), ((x+1,y), S), ((x,y-1), W), ((x,y+1), E)]
  where 
    h = length strs
    w = length (head strs)

opp :: Dir -> Dir
opp N = S
opp S = N
opp E = W
opp W = E
opp Z = Z