import Text.Regex.TDFA ( (=~), AllMatches(getAllMatches), AllTextMatches (getAllTextMatches) )
import Prelude hiding (lookup)

type Pos = (Int, Int)

main :: IO ()
main = do
  strs <- fmap lines getContents
  let paths = dfs strs [(0,1)]
  print $ maximum $ map (\ps -> length ps - 1) paths

dfs :: [String] -> [Pos] -> [[Pos]]
dfs strs ps
  | head ps == (length strs - 1, length (head strs) - 2) = [ps]
  | otherwise = concatMap (\s -> dfs strs (s:ps)) succs where
    curr = head ps
    succs = filter (`notElem` ps) $ findSuccs strs curr

findSuccs :: [String] -> Pos -> [Pos]
findSuccs strs (x,y) = concat [l,r,u,d] where
  l = [(x,y-1) | (y-1 >= 0) && strs !! x !! (y-1) `elem` ['.','<']]
  r = [(x,y+1) | (y+1 < length strs) && strs !! x !! (y+1) `elem` ['.','>']]
  u = [(x-1,y) | (x-1 >= 0) && strs !! (x-1) !! y `elem` ['.','^']]
  d = [(x+1,y) | (x+1 < length strs) && strs !! (x+1) !! y `elem` ['.','v']]
