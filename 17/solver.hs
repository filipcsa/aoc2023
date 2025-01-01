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
  let initPos = ((0,0), Z, 0) -- no initial dir faked by north
  let q = Q.singleton 0 initPos
  let cs = M.singleton initPos 0
  let (c, prevs) = dijkstra strs cs M.empty q
  print c

dijkstra :: [String] -> M.Map Pos Int -> M.Map Pos Pos -> Queue -> (Int, M.Map Pos Pos)
dijkstra strs costs prevs q
  | fst3 minCoord == (length strs - 1, length strs - 1) = (minCost, prevs)
  | otherwise = dijkstra strs costs' prevs' q' where
    (minCost, minCoord) = Q.findMin q
    q0 = Q.deleteMin q
    succs = filter (\(c,p) -> c < M.findWithDefault (10^6) p costs)
      $ map (\((x,y), d, n) -> (minCost + read [strs !! x !! y], ((x,y), d, n)))
      $ filter (\((x,y), d, n) -> n < 3)-- -> x /= invalidX && y /= invalidY && (x,y) /= minCoord)
      $ neighbors strs minCoord
    costs' = foldl (\acc (c,p) -> M.insert p c acc) costs succs
    prevs' = foldl (\acc (_,p) -> M.insert p minCoord acc) prevs succs
    q' = foldl (\acc (c,p) -> Q.insert c p acc) q0 succs

fst3 :: Pos -> (Int, Int) 
fst3 (c,_,_) = c

neighbors :: [String] -> Pos -> [Pos]
neighbors strs ((x,y), d, n) = 
  map (\(coor, d') -> if d' == d then (coor, d', n+1) else (coor, d', 0))
  $ filter (\((x',y'), d') -> x' >= 0 && x' < s && y' >= 0 && y' < s && d' /= opp d) 
    [((x-1, y), N), ((x+1,y), S), ((x,y-1), W), ((x,y+1), E)]
  where s = length strs

opp :: Dir -> Dir
opp N = S
opp S = N
opp E = W
opp W = E
opp Z = Z