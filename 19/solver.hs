import Text.Regex.TDFA ( (=~), AllMatches(getAllMatches), AllTextMatches (getAllTextMatches) )
import Prelude hiding (lookup)
import Data.List.Split (splitOn)
import Data.Map (Map, fromList, insert, lookup)
import Data.Maybe (fromJust)

data Part = Part { x :: Int, m :: Int, a :: Int, s :: Int } deriving Show

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

processPart :: Map String Workflow -> Part -> Workflow
processPart workflowByName = processPart' workflowByName "in"

processPart' :: Map String Workflow -> String -> Part -> Workflow
processPart' _ "A" _ = A
processPart' _ "R" _ = R
processPart' worflowByName currWorkflowName part =
  processPart' worflowByName nextWorkflowName part where
    workflow = fromJust $ lookup currWorkflowName worflowByName
    nextWorkflowName = processWorkflow workflow part

processWorkflow :: Workflow -> Part -> String
processWorkflow (Workflow name rules defaultWorkflow) part = nextWorkflowName where
  maybeNextWorkflowBasedOnRules = processRules rules part
  nextWorkflowName = case maybeNextWorkflowBasedOnRules of
    (Just next) -> next
    Nothing     -> defaultWorkflow

processRules :: [Rule] -> Part -> Maybe String
processRules [] _ = Nothing
processRules (rule:rules) part = maybeNext where
  processedRuleResult = processRule rule part
  maybeNext = case processedRuleResult of
    (Just next) -> Just next
    Nothing -> processRules rules part

processRule :: Rule -> Part -> Maybe String
processRule (Rule category compOp compVal targetWorkflow) part =
  if category part `compOp` compVal
    then Just targetWorkflow
    else Nothing

sumOfAcceptedRatings :: Int -> (Workflow, Part) -> Int
sumOfAcceptedRatings acc (R, _) = acc
sumOfAcceptedRatings acc (A, Part x m a s) = acc + x + m + a + s


main :: IO ()
main = do
  contents <- getContents
  let inputLines = lines contents
  let (workflows, parts) = parseInput inputLines
  let workflowByName = createWorkflowMap workflows
  let acceptOrReject = map (processPart workflowByName) parts
  print $ foldl sumOfAcceptedRatings 0 (zip acceptOrReject parts)


