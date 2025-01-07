import Text.Regex.TDFA ( (=~), AllMatches(getAllMatches), AllTextMatches (getAllTextMatches) )
import Prelude hiding (lookup)

main :: IO ()
main = do
  inputLines <- fmap lines getContents
  print 0