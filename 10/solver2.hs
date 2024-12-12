import Prelude hiding (lookup)
import qualified Data.Set as S

type Pos = (Int, Int)

pipeChars :: [Char]
pipeChars = ['|', '-', 'L', 'J', '7', 'F', 'S']

main :: IO ()
main = do
  contents <- getContents
  let layout = lines contents
  let r = length layout - 1
  let c = length (head layout) - 1
  let start = head $ [(x,y) | x <- [0..r], y <- [0..c], layout !! x !! y == 'S']
  let loop = dfsFindLoop layout start S.empty
  let nonLoop = [(x,y) | x <- [0..r], y <- [0..c], not $ (x,y) `S.member` loop]
  let enclosedOnes = filter (enclosed loop layout) nonLoop
  -- print enclosedOnes
  print $ length $ filter (enclosed loop layout) nonLoop

enclosed :: S.Set Pos -> [String] -> (Int, Int) -> Bool
enclosed loop strs pos =
  odd (countLoopCrossUp loop strs  pos) &&
  odd (countLoopCrossLeft loop strs pos)

countLoopCrossLeft :: S.Set Pos -> [String] -> (Int, Int) -> Int
countLoopCrossLeft loop strs (x,y) = horizontals + verticals where 
  verticals = length [(x, y') | y' <- [0..y], strs !! x !! y' == '|' && (x, y') `S.member` loop]
  hLs = [(x, y') | y' <- [0..y], strs !! x !! y' == 'L' && (x, y') `S.member` loop]
  hFs = [(x, y') | y' <- [0..y], strs !! x !! y' == 'F' && (x, y') `S.member` loop]
  horizontalLs = sum $ map (investigateRight '7' 'J' strs) hLs
  horizontalFs = sum $ map (investigateRight 'J' '7' strs) hFs
  horizontals = horizontalLs + horizontalFs

investigateRight :: Char -> Char -> [String] -> (Int, Int) -> Int
investigateRight goodChar badChar strs (x, y) = if strs !! cx !! cy == goodChar then 1 else 0 where
  (cx, cy) = head $ dropWhile (\(x',y') -> (strs !! x' !! y') `notElem` [goodChar, badChar]) $ [(x, y+i) | i <- [1..]]

countLoopCrossUp :: S.Set Pos -> [String] -> Pos -> Int
countLoopCrossUp loop strs (x, y) = verticals + horizontals where
  horizontals = length [(x', y) | x' <- [0..x], strs !! x' !! y == '-' && (x', y) `S.member` loop ]
  v7s = [(x', y) | x' <- [0..x], strs !! x' !! y == '7' && (x', y) `S.member` loop ]
  vFs = [(x', y) | x' <- [0..x], strs !! x' !! y == 'F' && (x', y) `S.member` loop ]
  vertical7s = sum $ map (investigateDown 'L' 'J' strs) v7s
  verticalFs = sum $ map (investigateDown 'J' 'L' strs) vFs
  verticals = vertical7s + verticalFs

investigateDown :: Char -> Char -> [String] -> (Int, Int) -> Int
investigateDown goodChar badChar strs (x, y) = if strs !! cx !! cy == goodChar then 1 else 0 where
  (cx, cy) = head $ dropWhile (\(x',y') -> (strs !! x' !! y') `notElem` [goodChar, badChar]) $ [(x+i, y) | i <- [1..]]

dfsFindLoop :: [String] -> Pos -> S.Set Pos -> S.Set Pos
dfsFindLoop strs pos visited
  | pos `S.member` visited = visited
  | otherwise = foldl
    (\acc nextPos -> S.union acc (dfsFindLoop strs nextPos acc))
    (S.insert pos visited)
    nextPoss where
      r = length strs
      c = length (head strs)
      nextPoss = filter (isPipe strs) $ generateNexts strs pos

isPipe :: [String] -> Pos -> Bool
isPipe strs (x, y) = x >= 0 && x < r && y >= 0 && y < c
  && strs !! x !! y `elem` pipeChars where
    r = length strs
    c = length (head strs)

generateNexts :: [String] -> Pos -> [Pos]
generateNexts strs (x, y) = case strs !! x !! y of
  'S' -> [(x+1, y), (x-1, y), (x, y+1), (x, y-1)]
  '|' -> [(x+1, y), (x-1, y)]
  '-' -> [(x, y-1), (x, y+1)]
  'L' -> [(x-1, y), (x, y+1)]
  'J' -> [(x-1, y), (x, y-1)]
  '7' -> [(x+1, y), (x, y-1)]
  'F' -> [(x+1, y), (x, y+1)]
