import qualified Data.Map as M
import qualified Data.Set as S

type Pos = (Int, Int)
type Succs = ([String] -> Pos -> [Pos])
type AdjMap = M.Map Pos [(Pos, Int)]

main :: IO ()
main = do
  strs <- fmap lines getContents
  let poss = [(x,y) | x <- [0..length strs - 1], y <- [0..length strs - 1], strs !! x !! y /= '#']
  let edges = concatMap (\p -> map (\s -> (p,s)) $ findSuccs2 strs p) poss
  let adjMap = foldl (\acc (f,t) -> M.insert f ((t,1):M.findWithDefault [] f acc) acc) M.empty edges
  let junctions = map fst $ filter (\(n, ss) -> length ss /= 2) $ M.toList adjMap
  let simpletons = map fst $ filter (\(n, ss) -> length ss == 2) $ M.toList adjMap
  let reduced = foldl reduceSimpleton adjMap simpletons
  let end = (length strs - 1, length (head strs) - 2)
  print $ maximum $ dfsLP reduced (0,1) 0 (S.singleton (0,1)) end

dfsLP :: AdjMap -> Pos -> Int -> S.Set Pos -> Pos -> [Int]
dfsLP adjs p l ps e
  | p == e = [l]
  | otherwise = concatMap (\(s,l') -> dfsLP adjs s (l+l') (S.insert s ps) e) succs where
    succs = filter (\(s,l') -> s `S.notMember` ps) $ M.findWithDefault [] p adjs

reduceSimpleton :: AdjMap -> Pos -> AdjMap
reduceSimpleton adjs simpleton = adjs' where
  [(suc1, n1), (suc2, n2)] = M.findWithDefault [] simpleton adjs
  adjs0 = M.delete simpleton adjs
  suc1succs = (suc2, n1+n2) : filter (\(s,n) -> s /= simpleton) (M.findWithDefault [] suc1 adjs)
  suc2succs = (suc1, n1+n2) : filter (\(s,n) -> s /= simpleton) (M.findWithDefault [] suc2 adjs)
  adjs' = M.insert suc2 suc2succs $ M.insert suc1 suc1succs adjs0


findSuccs2 :: Succs
findSuccs2 strs (x,y) = concat [l,r,u,d] where
  l = [(x,y-1) | (y-1 >= 0) && strs !! x !! (y-1) /= '#']
  r = [(x,y+1) | (y+1 < length strs) && strs !! x !! (y+1) /= '#']
  u = [(x-1,y) | (x-1 >= 0) && strs !! (x-1) !! y /= '#']
  d = [(x+1,y) | (x+1 < length strs) && strs !! (x+1) !! y /= '#']
