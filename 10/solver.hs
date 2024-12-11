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
  print $ S.size loop `div` 2

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
