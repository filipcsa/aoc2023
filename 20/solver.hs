import qualified Data.Graph as G
import qualified Data.Map as M
import Data.List.Split (splitOn)
import Dot (createAndStoreDotGraph)

data Pulse = High | Low deriving (Eq, Show)
type AdjMap = M.Map String [String]
type ModMap = M.Map String Char
type FlopMap = M.Map String Bool
type ConMap = M.Map String (M.Map String Pulse)
type Signal = (String, Pulse)
  
  -- let edges = concatMap (\(n, ns) -> [(n, n') | n' <- ns]) $ M.toList adjMap
  -- createAndStoreDotGraph edges modMap

main :: IO ()
main = do
  inputLines <- fmap lines getContents
  let (adjMap, modMap) = foldl processAdjs (M.empty, M.empty) inputLines
  let flopMap = M.fromList $ map (\(n,_) -> (n, False)) $ filter (\(_, c) -> c == '%') $ M.toList modMap
  let conMap = M.fromList $ map (\(n,_) -> (n, M.empty)) $ filter (\(_, c) -> c == '&') $ M.toList modMap
  print $ run adjMap [("broadcaster", Low)] (flopMap, conMap, []) 

run :: AdjMap -> [Signal] -> (FlopMap, ConMap, [Signal]) -> (FlopMap, ConMap, [Signal])
run _ [] r = r
run adjs ((n,p):ss) (flops, cons, sigs) = run adjs (ss ++ nexts) (flops', cons', (n,p):sigs) where
  (nexts, flops', cons') 
    | M.member n flops && p == High = ([], flops, cons)
    | M.member n flops && p == Low =
      if M.findWithDefault False n flops
        -- flop was on
        then (map (, Low) $ M.findWithDefault [] n adjs, M.insert n False flops, cons)
        -- flop was off
        else (map (, High) $ M.findWithDefault [] n adjs, M.insert n True flops, cons)
    -- | M.member n cons = 
    --   if all (==High) $ M. newCons
    --     then (map (,Low) $ M.findWithDefault [] n adjs, newCons)

processAdjs :: (AdjMap, ModMap) -> String -> (AdjMap, ModMap)
processAdjs (adjMap, modMap) str = (M.insert n ns adjMap, M.insert n t modMap)  where
  [nStr, nsStr] = splitOn " -> " str
  (n, t) = if nStr == "broadcaster" then (nStr, 'B') else (tail nStr, head nStr)
  ns = splitOn ", " nsStr