import Data.List.Split (splitOn)
import Data.List (sortBy)
import Data.Function (on)
import qualified Data.Map as M

type Pos = (Int, Int, Int)
type Brick = (Pos, Pos)

main :: IO ()
main = do
  inputLines <- fmap lines getContents
  let bricks = sortBy (compare `on` (third . snd)) $ map parseBrick inputLines
  let (brickByHeight, supports) = foldl processBrick (M.empty, M.empty) bricks
  -- let supportedBy = concatMap (\) M.toList supports
  print 0

processBrick :: (M.Map Int [Brick], M.Map Brick [Brick]) -> Brick -> (M.Map Int [Brick], M.Map Brick [Brick])
processBrick (bbh, ss) b
  | hasLanded b = (M.insert hb (b : M.findWithDefault [] hb bbh) bbh, M.insert b [] ss)
  | null supports = processBrick (bbh, ss) (moveDown b)
  | otherwise = (M.insert hb (b : M.findWithDefault [] hb bbh) bbh, M.insert b supports ss) where
    hb = snd $ getHeight b
    supports = getSupports bbh b 

getSupports :: M.Map Int [Brick] -> Brick -> [Brick]
getSupports bbh b = filter (overlap b) $ M.findWithDefault [] (fst (getHeight b) - 1) bbh

overlap :: Brick -> Brick -> Bool
overlap ((x1,y1,_),(x2,y2,_)) ((x'1,y'1,_),(x'2,y'2,_)) = 
  overlap' (x1,x2) (x'1,x'2) && overlap' (y1,y2) (y'1,y'2)

overlap' :: (Int, Int) -> (Int, Int) -> Bool
overlap' (s1,f1) (s2,f2) = max s1 s2 <= min f1 f2

getHeight :: Brick -> (Int, Int)
getHeight ((_,_,h1),(_,_,h2)) = (h1,h2)

moveDown :: Brick -> Brick
moveDown ((x1,y1,z1), (x2,y2,z2)) = ((x1,y1,z1-1), (x2,y2,z2-1))

hasLanded :: Brick -> Bool
hasLanded b = fst (getHeight b) == 1

third :: Pos -> Int
third (_,_,t) = t

parseBrick :: String -> Brick
parseBrick str = ((x1,y1,z1), (x2,y2,z2)) where
  [[x1,y1,z1], [x2,y2,z2]] = map (map read . splitOn ",") $ splitOn "~" str