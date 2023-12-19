import Text.Regex.TDFA ( (=~), AllMatches(getAllMatches), AllTextMatches (getAllTextMatches) )
import Prelude hiding (lookup)
import Data.List.Split (splitOn)
import Data.Map (Map, fromList, insert, lookup)
import Data.Maybe (fromJust)

data Part = Part { x :: Int, m :: Int, a :: Int, s :: Int } deriving Show

type Range = (Int, Int)
data PartRanges = PartRanges {
  xRange :: Range,
  mRange :: Range,
  aRange :: Range,
  sRange :: Range
} deriving Show

data Rule = Rule {
  category :: Part -> Int,
  compOp :: Int -> Int -> Bool,
  compVal :: Int,
  targetWorkflow :: String
}

instance Show Rule where
  show (Rule category compOp compVal targetWorkflow) =
    categoryStr ++ " " ++ compOpStr ++ " " ++ show compVal ++ " : " ++ targetWorkflow where
      categoryStr = case category $ Part 1 2 3 4 of
        1 -> "x"
        2 -> "m"
        3 -> "a"
        4 -> "s"
      compOpStr = if 1 `compOp` 2
        then "<" else ">"

data Workflow = A | R |
  Workflow {
     name :: String,
     rules :: [Rule],
     defaultWorkflow :: String
  } deriving Show


parseInput :: [String] -> ([Workflow], [Part])
parseInput lines = (workflows, parts) where
  [workflowLines, partLines] = splitOn [""] lines
  workflows = map parseWorkflow workflowLines
  parts = map parsePart partLines

parseWorkflow :: String -> Workflow
parseWorkflow str = Workflow name rules defaultWorkflow where
  name = takeWhile (/='{') str
  workflowStr = init $ tail $ dropWhile (/='{') str
  rulesStr = splitOn "," workflowStr
  rules = map parseRule $ init rulesStr
  defaultWorkflow = last rulesStr

parseRule :: String -> Rule
parseRule str = Rule category compOp compVal targetWorkflow where
  rp = "([xmas])([<>])([0-9]+):(.*)"
  (_,_,_,[categoryStr, compOpStr, compValStr, targetWorkflow]) = str =~ rp :: (String, String, String, [String])
  compVal = read compValStr
  category = case categoryStr of
    "x" -> x
    "m" -> m
    "a" -> a
    "s" -> s
  compOp = case compOpStr of
    "<" -> (<)
    ">" -> (>)

parsePart :: String -> Part
parsePart str = Part x m a s where
  rp = "{x=([0-9]+),m=([0-9]+),a=([0-9]+),s=([0-9]+)}"
  (_,_,_,categoryStrs) = str =~ rp :: (String, String, String, [String])
  categoryInts = map read categoryStrs :: [Int]
  [x,m,a,s]= categoryInts

createWorkflowMap :: [Workflow] -> Map String Workflow
createWorkflowMap workflows = workflowByName where
  accepRejectMap = fromList [("A", A), ("R", R)]
  workflowByName = foldl
    (\acc w -> insert (name w) w acc)
    accepRejectMap
    workflows

dfsRangeProcess :: Map String Workflow -> String -> PartRanges -> [PartRanges]
dfsRangeProcess _ "A" partRanges = [partRanges]
dfsRangeProcess _ "R" partRanges = []
dfsRangeProcess workflowByName workflowName partRanges =
  concat [dfsRangeProcess workflowByName oc pr | (oc, pr) <- zip outcomes partRangess] where
  (Workflow name rules defaultWorkflow) = fromJust $ lookup workflowName workflowByName
  outcomes = map targetWorkflow rules ++ [defaultWorkflow]
  partRangess = accumPartRangesForOutcomes rules partRanges

accumPartRangesForOutcomes :: [Rule] -> PartRanges -> [PartRanges]
accumPartRangesForOutcomes [] partRanges = [partRanges]
accumPartRangesForOutcomes (r:rs) partRanges =
  reducedRanges : accumPartRangesForOutcomes rs negatedReducedRanges where
    (reducedRanges, negatedReducedRanges) = reduceRanges r partRanges

reduceRanges :: Rule -> PartRanges -> (PartRanges, PartRanges)
reduceRanges (Rule category compOp compVal _) partRanges =
  (reducedRanges, negatedReducedRanges) where
    rangeToReduce = getRangeByCategory category partRanges
    reducedRange = reduceRange rangeToReduce compOp compVal
    negatedReducedRange = reduceRangeNegated rangeToReduce compOp compVal
    reducedRanges = setRangeByCategory category partRanges reducedRange
    negatedReducedRanges = setRangeByCategory category partRanges negatedReducedRange

getRangeByCategory :: (Part -> Int) -> PartRanges -> Range
getRangeByCategory category = getter where
  getter = case category (Part 1 2 3 4) of
    1 -> xRange
    2 -> mRange
    3 -> aRange
    4 -> sRange

setRangeByCategory :: (Part -> Int) -> PartRanges -> Range -> PartRanges
setRangeByCategory category (PartRanges xr mr ar sr) newRange = newRanges where
  newRanges = case category (Part 1 2 3 4) of
    1 -> PartRanges newRange mr ar sr
    2 -> PartRanges xr newRange ar sr
    3 -> PartRanges xr mr newRange sr
    4 -> PartRanges xr mr ar newRange

reduceRange :: Range -> (Int -> Int -> Bool) -> Int -> Range
reduceRange (l, u) compOp compVal = newRange where 
  nl = if l `compOp` compVal then l else compVal+1
  nu = if u `compOp` compVal then u else compVal-1
  newRange = (nl, nu)


reduceRangeNegated :: Range -> (Int -> Int -> Bool) -> Int -> Range
reduceRangeNegated (l, u) compOp compVal = newRange where 
  nl = if l `compOp` compVal then compVal else l
  nu = if u `compOp` compVal then compVal else u
  newRange = (nl, nu)

fullRanges :: PartRanges
fullRanges = PartRanges fr fr fr fr where 
  fr = (1,4000)

countCombinationSum :: [PartRanges] -> Int
countCombinationSum prs = sum $ map countCombinations prs

countCombinations :: PartRanges -> Int
countCombinations (PartRanges xr mr ar sr) = sss where
  sss = product $ map rangeCombs [xr, mr, ar, sr]

rangeCombs :: Range -> Int
rangeCombs (l, u) 
  | l > u = 0
  | otherwise = (u - l) + 1

main :: IO ()
main = do
  contents <- getContents
  let inputLines = lines contents
  let (workflows, parts) = parseInput inputLines
  let workflowByName = createWorkflowMap workflows
  let acceptedRanges = dfsRangeProcess workflowByName "in" fullRanges
  print acceptedRanges
  print $ countCombinationSum acceptedRanges

