import qualified Data.Set as S
import Data.List (sortBy)
import Data.Function (on)

type Pos = (Int, Int)
data Dir = N | W | S | E deriving Show
data World = World {
  walls :: S.Set Pos,
  balls :: S.Set Pos,
  ws :: Int
}

main :: IO ()
main = do
  contents <- getContents
  let inputLines = lines contents
  let initWorld = parseWorld inputLines
  let tiltedBalls = moveBalls initWorld N
  print $ totalLoad tiltedBalls

totalLoad :: World -> Int
totalLoad world = sum $ map (\(x,y) -> ws world - x + 1) $ S.toList (balls world)

moveBalls :: World -> Dir -> World
moveBalls world dir = world{balls=balls'} where
  balls' = foldl (moveBall world) (balls world) (sortBalls dir (balls world))

moveBall :: World -> S.Set Pos -> Pos -> S.Set Pos
moveBall world bs b = S.insert b' bs' where
  bs' = S.delete b bs
  b' = last
    $ takeWhile (\(x,y) -> x >= 0 && x <= ws world && y >= 0 && y <= ws world && (x,y) `S.notMember` walls world && (x,y) `S.notMember` bs')
    $ iterate (moveBall' N) b

moveBall' :: Dir -> (Int, Int) -> (Int, Int)
moveBall' N (x,y) = (x-1,y)

sortBalls :: Dir -> S.Set Pos -> [Pos]
sortBalls N bs = sortBy (compare `on` fst) $ S.toList bs

parseWorld :: [String] -> World
parseWorld strs = World (positions '#') (positions 'O') h where
  positions c = S.fromList [(x,y) | x <- [0..h], y <- [0..h], strs !! x !! y == c]
  h = length strs - 1
