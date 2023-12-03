import Data.Char (isNumber)
type Engine = [[Char]]
type Pos = (Int, Int)

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

getPartNumbers :: Engine -> [[Bool]] -> [Int]
getPartNumbers engine isAdjacent = partNums where
  zipped = zipWith zip engine isAdjacent
  numberGroups = concatMap zipLineFML zipped
  partGroups = filter isPartGroup numberGroups
  partNums = map group2Num partGroups

isPartGroup :: [(Char, Bool)] -> Bool
isPartGroup = any snd

group2Num :: [(Char, Bool)] -> Int
group2Num group = read $ map fst group

zipLineFML :: [(Char, Bool)] -> [[(Char, Bool)]]
zipLineFML [] = []
zipLineFML charBools = ttt where
  remaining = dropWhile (\(c,b) -> (not . isNumber) c) charBools
  ttt = if null remaining then [] else
    takeWhile (\(c,b) -> isNumber c) remaining : zipLineFML (dropWhile (\(c,b) -> isNumber c) remaining)

main :: IO ()
main = do
  contents <- getContents
  let engine = lines contents :: Engine
  let isAdjacentMap = getIsAdjacenctMap engine
  let partNumbers = getPartNumbers engine isAdjacentMap
  print $ sum partNumbers
  print 0