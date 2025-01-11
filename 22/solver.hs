import Data.List.Split (splitOn)
type Pos = (Int, Int, Int)
type Brick = (Pos, Pos)

main :: IO ()
main = do
  inputLines <- fmap lines getContents
  let bricks = map parseBrick inputLines
  -- todo sort by lower z
  -- let bricksByHeight = foldl 
  print bricks

parseBrick :: String -> Brick
parseBrick str = ((x1,y1,z1), (x2,y2,z2)) where
  [[x1,y1,z1], [x2,y2,z2]] = map (map read . splitOn ",") $ splitOn "~" str