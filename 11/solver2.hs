import Prelude hiding (lookup)
import Data.List (transpose)

type Galaxy = [[Char]]
type Pos = (Int, Int)

getExpansionIndices :: [String] -> ([Int], [Int])
getExpansionIndices galaxy = (getExpansionInDim galaxy 0, getExpansionInDim (transpose galaxy) 0)

getExpansionInDim :: Galaxy -> Int -> [Int]
getExpansionInDim [] _ = []
getExpansionInDim (gRow:gRows) idx = if withoutGalaxies gRow
  then idx : getExpansionInDim gRows (idx+1)
  else getExpansionInDim gRows (idx+1)

withoutGalaxies :: String -> Bool
withoutGalaxies str = and [c == '.' | c <- str]

getGalaxyPoss :: Galaxy -> [Pos]
getGalaxyPoss galaxy = [ (x,y) | x <- [0..gx], y <- [0..gy],
  (galaxy !! x) !! y == '#' ] where
    gx = length galaxy - 1
    gy = length (head galaxy) - 1

sumDists' :: [Pos] -> ([Int], [Int]) -> Int
sumDists' poss idxs = sum $ [ dist' idxs (poss !! x) (poss !! y) |
  x <- [0..(length poss - 2)],
  y <- [x+1..length poss - 1] ]

dist' :: ([Int], [Int]) -> Pos -> Pos -> Int
dist' (rei, cei) (ax, ay) (bx, by) = expDist where
  minx = min ax bx
  maxx = max ax bx
  miny = min ay by
  maxy = max ay by
  reis = [ r | r <- rei, r > minx && r < maxx ]
  ceis = [ c | c <- cei, c > miny && c < maxy ]
  origDist = abs (ax - bx) + abs (ay - by)
  rate = 999999
  expDist = origDist + (rate * length reis) + (rate * length ceis)

main :: IO ()
main = do
  contents <- getContents
  let inputLines = lines contents
  let galaxyPoss =  getGalaxyPoss inputLines
  let expansionIndices = getExpansionIndices inputLines
  print $ sumDists' galaxyPoss expansionIndices
