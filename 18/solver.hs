import qualified Data.PQueue.Prio.Min as Q
import qualified Data.Map as M
import Data.List (nub)

type Pos = (Int, Int)
type Queue = Q.MinPQueue Int Pos

main :: IO ()
main = do
  contents <- getContents
  let strs = lines contents
  let q = Q.singleton 0 (0,0) 
  let cs = M.singleton (0,0) 0
  let (c, prevs) = dijkstra strs cs M.empty q
  let p = retrievePath prevs (10^6) (Just (length strs - 1, length strs - 1))
  printPath strs p
  print c

printPath :: [String] -> [Pos] -> IO ()
printPath strs ps = sequence_ [print [if (x,y) `elem` ps then 'X' else strs !! x !! y | y <- [0..s]] | x <- [0..s]]
  where s = length strs - 1

dijkstra :: [String] -> M.Map Pos Int -> M.Map Pos Pos -> Queue -> (Int, M.Map Pos Pos)
dijkstra strs costs prevs q
  | minPos == (length strs - 1, length strs - 1) = (minCost, prevs)
  | otherwise = dijkstra strs costs' prevs' q' where
    (minCost, minPos) = Q.findMin q
    q0 = Q.deleteMin q
    (invalidX, invalidY) = dirChangeThing prevs minPos
    succs = filter (\(c,p) -> c < M.findWithDefault (10^6) p costs)
      $ map (\(x,y) -> (minCost + read [strs !! x !! y], (x,y)))
      $ filter (\(x,y) -> x /= invalidX && y /= invalidY && (x,y) /= minPos)
      $ neighbors strs minPos
    costs' = foldl (\acc (c,p) -> M.insert p c acc) costs succs
    prevs' = foldl (\acc (_,p) -> M.insert p minPos acc) prevs succs
    q' = foldl (\acc (c,p) -> Q.insert c p acc) q0 succs

dirChangeThing :: M.Map Pos Pos -> Pos -> (Int, Int)
dirChangeThing prevs pos
  | length last4 < 4 = (-1,-1)
  | length (nub $ map fst last4) == 1 = (fst $ head last4, -1)
  | length (nub $ map snd last4) == 1 = (-1, snd $ head last4)
  | otherwise = (-1,-1)
  where last4 = retrievePath prevs 4 (Just pos) 

retrievePath :: M.Map Pos Pos -> Int -> Maybe Pos -> [Pos]
retrievePath _ 0 _ = []
retrievePath _ _ Nothing = []
retrievePath prevs n (Just p) = retrievePath prevs (n-1) (M.lookup p prevs) ++ [p]

neighbors :: [String] -> Pos -> [Pos]
neighbors strs (x,y) = filter (\(x',y') -> x' >= 0 && x' < s && y' >= 0 && y' < s) [(x-1, y), (x+1,y), (x,y-1), (x,y+1)]
  where s = length strs
