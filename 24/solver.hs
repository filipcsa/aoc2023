import Data.List.Split (splitOn)
type Pos = (Int, Int, Int)
type Vel = (Int, Int, Int)

minArea :: Int
minArea = 7

maxArea :: Int
maxArea = 0

main :: IO ()
main = do
  inputLines <- fmap lines getContents
  let possAndVels = map parseInput inputLines
  let crossTimes = [ findCrossTime pv1 pv2 | pv1 <- possAndVels, pv2 <- possAndVels, pv1 /= pv2 ]
  print crossTimes
  -- print $ length $ filter validRange crossTimes

validRange :: Maybe (Int, Int) -> Bool
validRange Nothing = False
validRange _ = False

-- x1 + tvx1 = x2 + tvx2
-- t = (x1-x2) / (vx2 - vx1)
findCrossTime :: (Pos, Vel) -> (Pos, Vel) -> Maybe Double
findCrossTime ((x1,y1,_),(vx1,vy1,_)) ((x2,y2,_),(vx2,vy2,_)) = case (computeT x1 vx1 x2 vx2,computeT y1 vy1 y2 vy2) of
  (Just t1, Just t2) -> if abs (t1 - t2) < 10 then Just t1 else Nothing
  _ -> Nothing 

computeT :: Int -> Int -> Int -> Int -> Maybe Double
computeT x1 vx1 x2 vx2 = if vx1 == vx2 then Nothing else Just $ fromIntegral (x1-x2) / fromIntegral (vx2-vx1)

parseInput :: String -> (Pos, Vel)
parseInput str = ((x,y,z), (vx,vy,vz)) where
  [posStr, velStr] = splitOn " @ " str
  [x,y,z] = map read $ splitOn ", " posStr
  [vx,vy,vz] = map read $ splitOn ", " velStr
