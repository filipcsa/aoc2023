import Prelude hiding (lookup)
import Data.Char (ord)
import Data.List.Split (splitOn)
import Data.Map (Map, empty, insert, findWithDefault)

type Lense = (String, Int)

type Boxes = Map Int [Lense]

hash :: String -> Int
hash = foldl hashStep 0 

hashStep :: Int -> Char -> Int 
hashStep curr char = newVal `mod` 256 where 
  newVal = (curr + ord char) * 17

processStep :: Boxes -> String -> Boxes 
processStep boxes step = insert boxKey updatedBox boxes where
  boxOp = getBoxOp step
  lenseStr = takeWhile (/=boxOp) step 
  boxKey = hash lenseStr
  origBox = findWithDefault [] boxKey boxes
  updatedBox = if boxOp == '-'
    then removeLenseFromBox origBox lenseStr
    else addLenseToBox origBox step

getBoxOp :: String -> Char
getBoxOp (c:cs) | c == '-' || c == '=' = c
                | otherwise = getBoxOp cs

removeLenseFromBox :: [Lense] -> String -> [Lense]
removeLenseFromBox box lenseStr = filter (\l -> fst l /= lenseStr) box

addLenseToBox :: [Lense] -> String -> [Lense] 
addLenseToBox box str = addToBox box lense fl where 
  lense = takeWhile (/='=') str
  fl = read [last str] :: Int 

addToBox :: [Lense] -> String -> Int -> [Lense]
addToBox [] lense fl = [(lense, fl)]
addToBox (l:ls) lense fl 
  | fst l == lense = (lense, fl) : ls
  | otherwise = l : addToBox ls lense fl

computeFocusingPower :: Boxes -> Int 
computeFocusingPower boxes = foldl (boxFocusingPower boxes) 0 [0..255]

boxFocusingPower :: Boxes -> Int -> Int -> Int
boxFocusingPower boxes acc boxKey = foldl (lenseFocusingPower boxKey) acc slotBox where
  box = findWithDefault [] boxKey boxes 
  slotBox = zip [1..] box

lenseFocusingPower :: Int -> Int -> (Int, Lense) -> Int
lenseFocusingPower boxNum acc (slot, (lense, fl)) = 
  acc + (1+boxNum) * slot * fl

main :: IO ()
main = do
  contents <- getContents
  let inputLines = head $ lines contents
  let seqSteps = splitOn "," inputLines
  let finalBoxes = foldl processStep empty seqSteps
  print $ computeFocusingPower finalBoxes