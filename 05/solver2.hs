import Data.Map (Map, fromList, lookup)
import Data.List.Split (splitOn, chunksOf)
import Prelude hiding (lookup)

type Range = (Int, Int) -- both inclusive 
data Almanac = Almanac {
  seeds :: [Range],
  maps :: [[Range]]
} deriving Show

parseAlmanac :: [String] -> Almanac
parseAlmanac lines = Almanac { seeds = seeds, maps = maps } where
  strSegments = splitOn [""] lines
  seeds = parseSeeds ((head . head) strSegments)
  maps = parseMapRec (tail strSegments) seeds

parseMapRec :: [[String]] -> [Range] -> [[Range]]
parseMapRec [] targetRanges = [targetRanges]
parseMapRec (strMap:strMaps) sourceRanges = sourceRanges : parseMapRec strMaps targetRanges where
  vkrs = parseVKRs (tail strMap)-- value, key, range tuples
  targetRanges = concatMap (range2TargetRanges vkrs) sourceRanges

range2TargetRanges :: [(Int, Int, Int)] -> Range -> [Range]
range2TargetRanges [] (l, u) = if l < u then [(l, u)] else []
range2TargetRanges ((lv, lk, r):vkrs) (l, u)
  | u < lk || lk+r-1 < l = range2TargetRanges vkrs (l, u)
  | otherwise = targetRange : concatMap (range2TargetRanges vkrs) remainingSourceRanges where
    targetRange = getTargetRange (lv, lk, r) (l, u)
    remainingSourceRanges = getRemainingSources (lv, lk, r) (l, u)

getTargetRange :: (Int, Int, Int) -> Range -> Range
getTargetRange (v, k, r) (l, u) = (lv, lv + overlapSize) where
  kOverlapStart = max k l
  kOverlapFinish = min (k+r-1) u
  overlapSize = kOverlapFinish - kOverlapStart
  kShift = kOverlapStart - k
  lv = v + kShift

getRemainingSources :: (Int, Int, Int) -> Range -> [Range]
getRemainingSources (v, k, r) (l, u) = remainingSources where
  minL = min k l
  maxL = max k l
  minU = min (k+r-1) u
  maxU = max (k+r-1) u
  leftRemainder = [(l, maxL-1) | l < maxL]
  rightRemain = [(minU+1, maxU) | u > minU]
  remainingSources = leftRemainder ++ rightRemain

parseVKRs :: [String] -> [(Int, Int, Int)]
parseVKRs [] = []
parseVKRs (str:strss) = (v, k, r) : parseVKRs strss where
  numStrs = splitOn " " str
  [v, k, r] = map read numStrs :: [Int]

parseSeeds :: String -> [Range]
parseSeeds str = seeds where
  numStr = tail $ dropWhile (/=' ') str
  numStrs = splitOn " " numStr
  nums = map read numStrs
  pairs = chunksOf 2 nums
  seeds = [ (s, s+r-1) | [s,r] <- pairs ]

main :: IO ()
main = do
  contents <- getContents
  let inputLines = lines contents
  let almanac = parseAlmanac inputLines
  print $ maps almanac
  print $ minimum $ map fst (last $ maps almanac)