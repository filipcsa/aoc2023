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
  maps = foldl parseMap [] (tail strSegments)

parseSeeds :: String -> [Int]
parseSeeds str = seeds where
  numStr = tail $ dropWhile (/=' ') str
  numStrs = splitOn " " numStr
  seeds = map read numStrs

parseMap :: [Map Int Int] -> [String] -> [Map Int Int]
parseMap acc lines = acc ++ [fromList kvPairs] where
  kvPairs = concatMap parseKVRange (tail lines)

parseKVRange :: String -> [(Int, Int)]
parseKVRange str = kvPairs where
  numStrs = splitOn " " str
  [v, k, r] = map read numStrs :: [Int]
  kvPairs = [ (k+i,v+i) | i <- [0..(r-1)] ]

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