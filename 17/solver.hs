import Prelude hiding (lookup)
import Data.List (minimumBy)
import Data.Ord (comparing)

type Layout = [[Int]]
type Pos = (In