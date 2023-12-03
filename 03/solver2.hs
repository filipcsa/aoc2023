import Data.Char (isNumber)
import Data.Set (Set, fromList, member)
type Engine = [[Char]]
type Pos = (Int, Int)

data PartNum = PartNum {num :: Int, posSet :: Set Pos} deriving Show
type Gear = [PartNum]

getIsAdjacenctMap :: Engine -> [[Bool]]
getIsAdjacenctMap engine = [ [ isSymbolAdjacent (r,c) engine
  | c <- [0..w] ] | r <- [0..h]  ] where
    h = length engine - 1
    w = length (head engine) - 1

isSymbolAdjacent :: Pos -> Engine -> Bool
isSymbolAdjacent (pr, pc) engine = or [ isSymbol (pr+r,pc+c) engine | r <- [-1, 0, 1], c <- [-1, 0, 1], pr+r >= 0 && pc+c >= 0 && pr+r < h && pc+c < w] where
      h = length engine
      w = length $ head engine

isSymbol :: Pos -> Engine -> Bool
isSymbol (r,c) engine = (not . isNumber) char && char /= '.' where
  char = (engine !! r) !! c

getPartNumbers :: Engine -> [[Bool]] -> [PartNum]
getPartNumbers engine isAdjacent = partNums where
  poss = [ [( r, c) | c <- [0..w]] | r <- [0..h]]
  h = length engine - 1
  w = length $ head engine
  zipped = map (\(cs, bs, ps) -> zip3 cs bs ps) (zip3 engine isAdjacent poss)
  numberGroups = concatMap zipLineFML zipped
  partGroups = filter isPartGroup numberGroups
  partNums = map group2PartNum partGroups

isPartGroup :: [(Char, Bool, Pos)] -> Bool
isPartGroup = any (\(c,b,p) -> b)

group2PartNum :: [(Char, Bool, Pos)] -> PartNum
group2PartNum group = PartNum { num=n, posSet=ps } where
  n = read $ map (\(c,b,p) -> c) group :: Int
  ps = fromList $ map (\(c,b,p) -> p) group

zipLineFML :: [(Char, Bool, (Int, Int))] -> [[(Char, Bool, (Int, Int))]]
zipLineFML [] = []
zipLineFML charBools = ttt where
  remaining = dropWhile (\(c,b,p) -> (not . isNumber) c) charBools
  ttt = if null remaining then [] else
    takeWhile (\(c,b,p) -> isNumber c) remaining : zipLineFML (dropWhile (\(c,b,p) -> isNumber c) remaining)

main :: IO ()
main = do
  contents <- getContents
  let engine = lines contents :: Engine
  let isAdjacentMap = getIsAdjacenctMap engine
  let partNumbers = getPartNumbers engine isAdjacentMap
  let gears = getGears engine partNumbers
  print $ sum $ map (product . map num) gears
  print 0

getGears :: Engine -> [PartNum] -> [Gear]
getGears engine partNums = ggg where
  poss = [ [( r, c) | c <- [0..w]] | r <- [0..h]]
  h = length engine - 1
  w = length $ head engine
  zz = zipWith zip engine poss
  gearCandidates = concatMap (filter (\(c,p) -> c == '*')) zz
  gcAdjacentPartNums = map (adjacentPartNums partNums) gearCandidates
  ggg = filter (\ps -> length ps == 2) gcAdjacentPartNums

adjacentPartNums :: [PartNum] -> (Char, Pos) -> [PartNum]
adjacentPartNums partNums (_, pos) = filter (isPartNumAdjacenct pos) partNums

isPartNumAdjacenct :: Pos -> PartNum -> Bool
isPartNumAdjacenct (pr, pc) partNum = b where
  posAdjs = [ (pr+r,pc+c) | r <- [-1, 0, 1], c <- [-1, 0, 1], pr+r >= 0 && pc+c >= 0 ]
  b = any (\n -> member n (posSet partNum)) posAdjs

