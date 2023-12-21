import Prelude hiding (lookup)
import Data.List.Split (splitOn)
import Data.Set (fromList, toList, Set, member, insert)
import Data.Foldable (minimumBy, maximumBy)
import Data.Ord (comparing)
import Data.List (sort, nub)

data Dig = Dig { dir :: Char, dirLen :: Int, color :: String }
  deriving Show
type Pos = (Int, Int)

parseDig :: String -> Dig
parseDig str = Dig { dir=dir, dirLen=dirLen, color=color } where
  [dirStr, dirLenStr, color] = splitOn " " str
  dir = head dirStr
  dirLen = read dirLenStr :: Int

digTrenchOutline :: [Dig] -> [Pos]
digTrenchOutline = foldl digLine [(0,0)]

digLine :: [Pos] -> Dig -> [Pos]
digLine accTrench dig = digLineInDir (dir dig) (dirLen dig) accTrench

digLineInDir :: Char -> Int -> [Pos] -> [Pos]
digLineInDir _ 0 poss = poss
digLineInDir d n poss = digLineInDir d (n-1) (poss ++ [hole]) where
  (x, y) = last poss
  hole = case d of
    'R' -> (x,y+1)
    'L' -> (x,y-1)
    'D' -> (x+1,y)
    'U' -> (x-1,y)

digTrench :: Set Pos -> Set Pos
digTrench outlineSet = bfs [innerPos] (insert innerPos outlineSet) where
  minX = fst $ minimumBy (comparing fst) outlineSet
  maxX = fst $ maximumBy (comparing fst) outlineSet
  minY = snd $ minimumBy (comparing snd) outlineSet
  maxY = snd $ maximumBy (comparing snd) outlineSet
  fillCandidates = [ (x,y) | x <- [minX..maxX], y <- [minY..maxY] ]
  innerPos = head $ filter (isInOutline outlineSet) fillCandidates

bfs :: [Pos] -> Set Pos -> Set Pos
bfs [] poss = poss
bfs currs poss = bfs nexts updatedPoss where
  nextCandidates = nub $ concat [ [(x+1,y), (x-1,y), (x,y+1), (x,y-1)] | (x,y) <- currs ]
  nexts = filter (\p -> not (member p poss)) nextCandidates
  updatedPoss = foldl (flip insert) poss nexts

-- when the number of outline poss above is odd and the number of left outlines is odd, the pos is in the trench
isInOutline :: Set Pos -> Pos -> Bool
isInOutline outlineSet (x,y) = top && bot && left && right where
  isOutline = member (x,y) outlineSet
  top = raycastUp outlineSet (x,y)
  bot = raycastDown outlineSet (x,y)
  left = raycastLeft outlineSet (x,y)
  right = raycastRight outlineSet (x,y)

raycastUp :: Set Pos -> Pos -> Bool
raycastUp outlineSet (x,y) = numSegments `mod` 2 == 1 where
  sortedTopPoss =
    sort $
    map fst $
    filter (\(ox, oy) -> oy == y && ox < x) (toList outlineSet)
  numSegments = countSegments sortedTopPoss

raycastDown :: Set Pos -> Pos -> Bool
raycastDown outlineSet (x,y) = numSegments `mod` 2 == 1 where
  sortedTopPoss =
    sort $
    map fst $
    filter (\(ox, oy) -> oy == y && ox > x) (toList outlineSet)
  numSegments = countSegments sortedTopPoss

raycastLeft :: Set Pos -> Pos -> Bool
raycastLeft outlineSet (x,y) = numSegments `mod` 2 == 1 where
  sortedLeftPoss =
    sort $
    map snd $
    filter (\(ox, oy) -> ox == x && oy < y) (toList outlineSet)
  numSegments = countSegments sortedLeftPoss

raycastRight :: Set Pos -> Pos -> Bool
raycastRight outlineSet (x,y) = numSegments `mod` 2 == 1 where
  sortedLeftPoss =
    sort $
    map snd $
    filter (\(ox, oy) -> ox == x && oy > y) (toList outlineSet)
  numSegments = countSegments sortedLeftPoss

countSegments :: [Int] -> Int
countSegments [] = 0
countSegments (x:xs) = 1 + countSegments' x xs

countSegments' :: Int -> [Int] -> Int
countSegments' _ [] = 0
countSegments' prev (x:xs)
  | abs (prev - x) == 1 = countSegments' x xs
  | otherwise = 1 + countSegments' x xs


printTrench :: Set Pos -> IO ()
printTrench poss = sequence_ [ printTrenchLine x minY maxY poss | x <- [minX..maxX] ]
  where
      minX = fst $ minimumBy (comparing fst) poss
      maxX = fst $ maximumBy (comparing fst) poss
      minY = snd $ minimumBy (comparing snd) poss
      maxY = snd $ maximumBy (comparing snd) poss

printTrenchLine :: Int -> Int -> Int -> Set Pos -> IO ()
printTrenchLine x minY maxY poss =
  print [ if member (x, y) poss then '#' else '.' | y <- [minY..maxY] ]


main :: IO ()
main = do
  contents <- getContents
  let inputLines = lines contents
  let digPlan = map parseDig inputLines
  print digPlan
  let trenchOutline = fromList $ digTrenchOutline digPlan
  print $ length trenchOutline
  -- printTrench trenchOutline
  print "============"
  let trench = digTrench trenchOutline
  -- printTrench trench
  print $ length trench