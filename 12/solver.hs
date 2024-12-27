import Data.List.Split (splitOn)
import qualified Data.Map as M
import Data.List (intercalate)

type Record = (String, [Int])
type Mem = M.Map (Int, Int) Int

main :: IO ()
main = do
  contents <- getContents
  let inputLines = lines contents
  let records = map parseRecord inputLines
  let mems = map run' records
  let possibles = map getPossibles mems
  let expandedRecords = map expandRecord records
  let expandedMems = map run' expandedRecords
  let expandedPossibles = map getPossibles expandedMems
  print $ sum possibles
  print $ sum expandedPossibles

expandRecord :: Record -> Record
expandRecord (bs,ss) = (intercalate "?" (replicate 5 bs), concat (replicate 5 ss))

getPossibles :: Mem -> Int
getPossibles mem = sum $ map snd $ filter (\((_,si),_) -> si == 0) $ M.toList mem

run' :: Record -> Mem
run' (bs,ss) = foldl (\acc bi -> run (bs,ss) bi 0 acc) M.empty [0..maxBi] where
  maxBi = length (takeWhile (/='#') bs)

run :: Record -> Int -> Int -> Mem -> Mem
run (bs,ss) bi si mem
  | (bi,si) `M.member` mem = mem
  | not blockFits = M.insert (bi,si) 0 mem
  | si == length ss - 1 && elem '#' (drop minBi bs) = M.insert (bi,si) 0 mem
  | si == length ss - 1 = M.insert (bi,si) 1 mem
  | otherwise = M.insert (bi,si) sumBelow mem' where
    blockFits = doesBlockFit bs bi (ss !! si) --length (drop bi bs) >= (ss !! si) && (all (`elem` ['?','#']) $ take (ss !! si) $ drop bi bs)
    minBi = bi + (ss !! si) + 1
    maxBiD = length $ takeWhile (/='#') $ drop minBi bs
    nextBis = [minBi .. minBi+maxBiD]
    mem' = foldl (\acc bi' -> run (bs,ss) bi' (si+1) acc) mem nextBis
    sumBelow = sum [M.findWithDefault 0 (bi',si+1) mem' | bi' <- nextBis]

doesBlockFit :: String -> Int -> Int -> Bool
doesBlockFit bs bi s = length (drop bi bs) >= s 
  && all (`elem` ['?','#']) (take s $ drop bi bs) 
  && (length bs <= (bi + s) || bs !! (bi + s) /= '#')

parseRecord :: String -> Record
parseRecord str = (blockStr, nums) where
  [blockStr, numsStr] = splitOn " " str
  nums = map read $ splitOn "," numsStr
