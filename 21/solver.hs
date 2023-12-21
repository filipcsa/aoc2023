import Text.Regex.TDFA ( (=~), AllMatches(getAllMatches), AllTextMatches (getAllTextMatches) )
import Prelude hiding (lookup)
import Data.List (nub, nubBy)
import Data.Set (fromList, Set, member)

type Layout = [[Char]]
type Pos = (Int, Int)

data Path =
    Source { pos :: Pos }
  | Node { pos :: Pos, prev :: Path }

findStartingPos :: Layout -> Pos
findStartingPos layout =
  head $ [ (x,y) | x <- [0..h-1], y <- [0..w-1], layout !! x !! y == 'S' ] where
    h = length layout
    w = length $ head layout

getReachablePossInSteps :: Layout -> Pos -> Int -> [Pos]
getReachablePossInSteps layout start ste