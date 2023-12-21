import Text.Regex.TDFA ( (=~), AllMatches(getAllMatches), AllTextMatches (getAllTextMatches) )
import Prelude hiding (lookup)

main :: IO ()
main = do
  contents <- getContents
  let inputLines = lines contents
  print 0