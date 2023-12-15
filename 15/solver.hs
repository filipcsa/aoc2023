import Prelude hiding (lookup)
import Data.Char (ord)
import Data.List.Split (splitOn)

hash :: String -> Int
hash = foldl hashStep 0 

hashStep :: Int -> Char -> Int 
hashStep curr char = newVal `mod` 256 where 
  newVal = (curr + ord char) * 17

main :: IO ()
main = do
  contents <- getContents
  let inputLines = head $ lines contents
  let seqSteps = splitOn "," inputLines
  let hashes = map hash seqSteps
  print $ sum hashes