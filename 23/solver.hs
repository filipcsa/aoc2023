import qualified Data.Map as M
import qualified Data.Set as S

type Pos = (Int, Int)

main :: IO ()
main = do
  strs <- fmap lines getContents
  print $ maximum $ map (\ps -> length ps - 1) $ dfs strs (S.singleton (0,1)) (0,1) findSuccs1
  print $ maximum $ map (\ps -> length ps - 1) $ dfs strs (S.singleton (0,1)) (0,1) findSuccs2

dfs :: [String] -> S.Set Pos -> Pos -> ([String] -> Pos -> [Pos]) -> [S.Set Pos]
dfs strs ps p fs
  | p == (length strs - 1, length (head strs) - 2) = [ps]
  | otherwise = concatMap (\s -> dfs strs (S.insert s ps) s fs) succs where
    succs = filter (`S.notMember` ps) $ fs strs p

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
