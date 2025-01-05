import Prelude hiding (lookup)
import Data.List.Split (splitOn)

type Move = (Char, Int)
type Coord = (Int, Int)

main :: IO ()
main = do
  contents <- getContents
  let inputLines = lines contents
  let moves = map parseMove inputLines
  let coords = foldl makeCoord [(0,0)] moves
  print $ computeArea coords

computeArea :: [Coord] -> Int
computeArea coords = a' + (c `div` 2) + 1 where
  a' = sum $ zipWith (curry (\((x0, y0), (x1, y1)) -> ((y0+y1) * (x0 - x1)) `div` 2)) (init coords) (tail coords)
  c = sum  $ zipWith (curry (\((x0, y0), (x1, y1)) -> abs (y0-y1) + abs (x0-x1))) (init coords) (tail coords)

makeCoord :: [Coord] -> Move -> [Coord]
makeCoord poss move = poss ++ [makeCoord' (last poss) move]

makeCoord' :: Coord -> Move -> Coord
makeCoord' (x, y) ('R', n) = (x+n, y)
makeCoord' (x, y) ('L', n) = (x-n, y)
makeCoord' (x, y) ('D', n) = (x, y+n)
makeCoord' (x, y) ('U', n) = (x, y-n)

parseMove :: String -> Move
parseMove str = (head dirStr, read numStr) where
  [dirStr, numStr, _] = splitOn " " str

parseMove' :: String -> Move
parseMove' str = (head dirStr, read numStr) where
  [dirStr, numStr, _] = splitOn " " str
