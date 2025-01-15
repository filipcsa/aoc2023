import qualified Data.Map as M
import qualified Data.Set as S

type Pos = (Int, Int)
type Edge = (Pos, Pos, Int)

main :: IO ()
main = do
  strs <- fmap lines getContents
  let edges = dfsEdges strs findSuccs1 (0,1) (0,1) (S.singleton (0,1)) 
  print 0

findSuccs1 :: [String] -> Pos -> [Pos]
findSuccs1 strs (x,y) = concat [l,r,u,d] where
  l = [(x,y-1) | (y-1 >= 0) && strs !! x !! (y-1) `elem` ['.','<']]
  r = [(x,y+1) | (y+1 < length strs) && strs !! x !! (y+1) `elem` ['.','>']]
  u = [(x-1,y) | (x-1 >= 0) && strs !! (x-1) !! y `elem` ['.','^']]
  d = [(x+1,y) | (x+1 < length strs) && strs !! (x+1) !! y `elem` ['.','v']]

findSuccs2 :: [String] -> Pos -> [Pos]
findSuccs2 strs (x,y) = concat [l,r,u,d] where
  l = [(x,y-1) | (y-1 >= 0) && strs !! x !! (y-1) /= '#']
  r = [(x,y+1) | (y+1 < length strs) && strs !! x !! (y+1) /= '#']
  u = [(x-1,y) | (x-1 >= 0) && strs !! (x-1) !! y /= '#']
  d = [(x+1,y) | (x+1 < length strs) && strs !! (x+1) !! y /= '#']
