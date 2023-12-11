import Prelude hiding (lookup)
import Data.List.Split (splitOn)

parseSequence :: String -> [Int]
parseSequence str = map read (splitOn " " str)

diffTillZero :: [Int] -> [[Int]]
diffTillZero seq
  | allZeros seq = [seq]
  | otherwise = seq : diffTillZero diffSeq where
    subSeqPairs = zip seq (tail seq)
    diffSeq = map (\(a,b) -> b-a) subSeqPairs

allZeros :: [Int] -> Bool
allZeros seq = and [ n == 0 | n <- seq ]

nextInSequence :: [[Int]] -> Int
nextInSequence diffTillZero = next where
  zeroToOrig = tail $ reverse diffTillZero
  next = foldl addLast 0 zeroToOrig

addLast :: Int -> [Int] -> Int
addLast acc seq = acc + last seq

main :: IO ()
main = do
  contents <- getContents
  let inputLines = lines contents
  let sequences = map parseSequence inputLines
  print sequences
  let diffsTillZero = map diffTillZero sequences
  print diffsTillZero
  let nextsInSequences = map nextInSequence diffsTillZero
  print $ sum nextsInSequences
