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
  print $ cycleLength initWorld

cycleLength :: World -> Int
cycleLength world = num where
  singleCyclePath = cyclePath [world]
  initNumCycles = snd $ head $ filter (\(w,i) -> balls w == balls (last singleCyclePath)) $ zip singleCyclePath [1..]
  cLength = length singleCyclePath - initNumCycles
  idx = initNumCycles + ((10^9 - initNumCycles) `mod` cLength)
  num = totalLoad (singleCyclePath !! idx)

cyclePath :: [World] -> [World]
cyclePath ws
  | any (\w -> balls w == balls w') ws = ws ++ [w']
  | otherwise = cyclePath (ws ++ [w'])
  where w'= runCycle (last ws)

runCycle :: World -> World
runCycle world = do
  let nWorld = moveBalls world N
  let wWorld = moveBalls nWorld W
  let sWorld = moveBalls wWorld S
  let eWorld = moveBalls sWorld E
  eWorld

totalLoad :: World -> Int
totalLoad world = sum $ map (\(x,y) -> ws world - x + 1) $ S.toList (balls world)

moveBalls :: World -> Dir -> World
moveBalls world dir = world{balls=balls'} where
  balls' = foldl (moveBall world dir) (balls world) (sortBalls dir (balls world))

moveBall :: World -> Dir -> S.Set Pos -> Pos -> S.Set Pos
moveBall world dir bs b = S.insert b' bs' where
  bs' = S.delete b bs
  b' = last
    $ takeWhile (\(x,y) -> x >= 0 && x <= ws world && y >= 0 && y <= ws world && (x,y) `S.notMember` walls world && (x,y) `S.notMember` bs')
    $ iterate (moveBall' dir) b

moveBall' :: Dir -> (Int, Int) -> (Int, Int)
moveBall' N (x,y) = (x-1,y)
moveBall' S (x,y) = (x+1,y)
moveBall' W (x,y) = (x,y-1)
moveBall' E (x,y) = (x,y+1)

sortBalls :: Dir -> S.Set Pos -> [Pos]
sortBalls N bs = sortBy (compare `on` fst) $ S.toList bs
sortBalls S bs = reverse $ sortBy (compare `on` fst) $ S.toList bs
sortBalls W bs = sortBy (compare `on` snd) $ S.toList bs
sortBalls E bs = reverse $ sortBy (compare `on` snd) $ S.toList bs

parseWorld :: [String] -> World
parseWorld strs = World (positions '#') (positions 'O') h where
  positions c = S.fromList [(x,y) | x <- [0..h], y <- [0..h], strs !! x !! y == c]
  h = length strs - 1
