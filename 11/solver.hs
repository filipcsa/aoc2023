import Prelude hiding (lookup)
import Data.List (transpose)

type Galaxy = [[Char]]
type Pos = (Int, Int)

expandGalaxy :: [String] -> Galaxy
expandGalaxy orig = transpose $ expandGalaxyDimension $ transpose $ expandGalaxyDimension orig

expandGalaxyDimension :: Galaxy -> Galaxy
expandGalaxyDimension [] = []
expandGalaxyDimension (gRow:gRows) = possiblyExpandedRow
  ++ expandGalaxyDimension gRows where
    possiblyExpandedRow = if withoutGalaxies gRow 
      then [gRow, gRow] 
      else [gRow]

withoutGalaxies :: String -> Bool
withoutGalaxies str = and [c == '.' | c <- str]

getGalaxyPoss :: Galaxy -> [Pos]
getGalaxyPoss galaxy = [ (x,y) | x <- [0..gx], y <- [0..gy], 
  (galaxy !! x) !! y == '#' ] where 
    gx = length galaxy - 1
    gy = length (head galaxy) - 1

sumDists :: [Pos] -> Int
sumDists poss = sum $ [ dist (poss !! x) (poss !! y) | 
  x <- [0..(length poss - 2)],
  y <- [x+1..length poss - 1] ]

dist :: Pos -> Pos -> Int
dist (ax, ay) (bx, by) = abs (ax - bx) + abs (ay - by)

main :: IO ()
main = do
  contents <- getContents
  let inputLines = lines contents
  let expandedGalaxy = expandGalaxy inputLines
  sequence_ [print gRow | gRow <- expandedGalaxy]
  let galaxyPoss =  getGalaxyPoss expandedGalaxy
  print galaxyPoss
  print $ sumDists galaxyPoss

