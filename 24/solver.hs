import Data.List.Split (splitOn)
import GHC.CmmToAsm.AArch64.Instr (_d)
type Pos = (Int, Int, Int)
type Vel = (Int, Int, Int)

minArea :: Double
minArea = 200000000000000

maxArea :: Double
maxArea = 400000000000000

main :: IO ()
main = do
  inputLines <- fmap lines getContents
  let possAndVels = map parseInput inputLines
  let crossTimes = concat [ [findCrossPos (possAndVels !! i) (possAndVels !! j) | j <- [i+1..length possAndVels - 1]] | i <- [0..length possAndVels - 1]]
  print $ length $ filter validRange crossTimes

validRange :: (Double, Double) -> Bool
validRange (x,y) = minArea <= x && x <= maxArea && minArea <= y && y <= maxArea

findCrossPos :: (Pos, Vel) -> (Pos, Vel) -> (Double, Double)
findCrossPos ((x1,y1,_),(vx1,vy1,_)) ((x2,y2,_),(vx2,vy2,_d)) = 
  if t >= 0 && l >= 0 then (xcross, ycross) else (-1,-1) where
  t = fromIntegral (vx2*(y2-y1) -vy2*(x2-x1)) / fromIntegral (vx2*vy1 - vx1*vy2)
  l = fromIntegral (vx1*(y2-y1) -vy1*(x2-x1)) / fromIntegral (vx2*vy1 - vx1*vy2)
  xcross = fromIntegral x1 + t * fromIntegral vx1
  ycross = fromIntegral y1 + t * fromIntegral vy1

parseInput :: String -> (Pos, Vel)
parseInput str = ((x,y,z), (vx,vy,vz)) where
  [posStr, velStr] = splitOn " @ " str
  [x,y,z] = map read $ splitOn ", " posStr
  [vx,vy,vz] = map read $ splitOn ", " velStr
