import Prelude hiding (lookup)
import Data.List.Split (splitOn)

main :: IO ()
main = do
  contents <- getContents
  let inputLines = lines contents
  print 0
