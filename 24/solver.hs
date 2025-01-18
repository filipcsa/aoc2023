import Data.List.Split (splitOn)
type Pos = (Int, Int, Int)
type Vel = (Int, Int, Int)

main :: IO ()
main = do
  inputLines <- fmap lines getContents
  let possAndVels = map parseInput inputLines
  let crossPos2D = [ findCrossPos pv1 pv2 | pv1 <- possAndVels, pv2 <- possAndVels, pv1 /= pv2 ]
  print $ length $ filter validRange crossPos2D

validRange :: Maybe (Int, Int) -> Bool
validRange Nothing = False
validRange _ = False

findCrossPos :: (Pos, Vel) -> (Pos, Vel) -> Maybe (Int, Int)
findCrossPos _ _ = Nothing

parseInput :: String -> (Pos, Vel)
parseInput str = ((x,y,z), (vx,vy,vz)) where
  [posStr, velStr] = splitOn " @ " str
  [x,y,z] = map read $ splitOn ", " posStr
  [vx,vy,vz] = map read $ splitOn ", " velStr
