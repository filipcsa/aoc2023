import Prelude hiding (lookup)
import Data.List.Split (splitOn)
import Data.Set (fromList, toList, Set, member, insert)
import Data.Foldable (minimumBy, maximumBy)
import Data.Ord (comparing)
import Data.List (sort, nub)

data Dig = Dig { dir :: Char, dirLen :: Int, color :: String }
  deriving Show
type Pos = (Int, Int)

parseDig :: String -> Dig
parseDig str = Dig { dir=dir, dirLen=dirLen, color=color } where
  [dirStr, dirLenStr, color] = 