import Data.Map (Map, fromList, lookup)
import Data.List.Split (splitOn)
import Prelude hiding (lookup)

data Almanac = Almanac {
  seeds :: [Int],
  maps :: [Map Int Int]
} deriving Show

parseAlmanac :: [String] -> Almanac
parseAlmanac lines = Almanac { seeds = seeds, maps = maps } where
  strSegments = splitOn [""] lines
  seeds = parseSeeds ((head . head) strSegments)
  maps = parseMapRec (tail strSegments) seeds
  
parseMapRec :: [[String]] -> [Int] -> [Map Int Int]
parseMapRec [] _ = []
parseMapRec (strMap:strMaps) keys = m : parseMapRec strMaps vals where
  vkrs = parseVKRs (tail strMap)-- value, key, range tuples
  vals = map (key2Val vkrs) keys
  m = fromList $ zip keys vals

key2Val :: [(Int, Int, Int)] -> Int -> Int
key2Val vkrs key = val where
  vkr = filter (\(v, k, r) -> key >= k && key < k + r) vkrs
  val = case vkr of
    [] -> key
    ((v,k,r):_) -> key - (k-v)

parseVKRs :: [String] -> [(Int, Int, Int)]
parseVKRs [] = []
parseVKRs (str:strss) = (v, k, r) : parseVKRs strss where 
  numStrs = splitOn " " str
  [v, k, r] = map read numStrs :: [Int]

parseSeeds :: String -> [Int]
parseSeeds str = seeds where
  numStr = tail $ dropWhile (/=' ') str
  numStrs = splitOn " " numStr
  seeds = map read numStrs

lookupWithDefault :: Map Int Int -> Int -> Int
lookupWithDefault m k = case lookup k m of
  (Just v)  -> v
  Nothing   -> k

seed2Location :: Almanac -> [Int]
seed2Location almanac = foldl lookupNums (seeds almanac) (maps almanac)

lookupNums :: [Int] -> Map Int Int -> [Int]
lookupNums ks m = map (lookupWithDefault m) ks

main :: IO ()
main = do
  contents <- getContents
  let inputLines = lines contents
  let almanac = parseAlmanac inputLines
  let locations = seed2Location almanac
  print $ minimum locations