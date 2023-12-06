main :: IO ()
main = do
  contents <- getContents
  let inputLines = lines contents
  print 0

lstrip :: String -> String
lstrip = dropWhile (==' ')