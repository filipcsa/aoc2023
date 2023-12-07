import Prelude hiding (lookup)

main :: IO ()
main = do
  contents <- getContents
  let inputLines = lines contents
  print 0

lstrip :: String -> String
lstrip = dropWhile (==' ')

-- qsort with comparator which returns true if first out of two is smaller
qsort :: Ord a => (a -> a -> Bool) -> [a] -> [a]
qsort isSmaller (x:xs) = smaller ++ [x] ++ larger where
  smaller = [ s | s <- xs, isSmaller s x ]
  larger = [ l | l <- xs, isSmaller x l ]