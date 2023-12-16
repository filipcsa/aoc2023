import Prelude hiding (lookup)
import Data.List (transpose)

type Layout = [[Char]]

slideNorth :: Layout -> Layout
slideNorth layout = transpose $ map leftShiftLine (transpose layout)

leftShiftLine :: [Char] -> [Char]
leftShiftLine orig = if orig == shiftedLine
  then orig
  else leftShiftLine shiftedLine where 
    shiftedLine = performShift orig 

performShift :: [Char] -> [Char]
performShift [] = []
performShift ('.':'O':rest) = 'O' : performShift ('.':rest)
performShift (c:cs) = c : performShift cs

computeLoad :: Layout -> Int 
computeLoad layout = foldl computeRowLoad 0 (zip layout weights) where 
  weights = reverse [1..length layout]

computeRowLoad :: Int -> ([Char], Int) -> Int
computeRowLoad acc (row, weight) = acc + rocks * weight where 
  rocks = length $ filter (=='O') row

main :: IO ()
main = do
  contents <- getContents
  let layout = lines contents :: Layout
  let northLayout = slideNorth layout
  print $ computeLoad northLayout
  