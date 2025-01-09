import qualified Data.Graph as G
import qualified Data.Map as M
import Data.List.Split (splitOn)
import Dot (createAndStoreDotGraph)

data Pulse = High | Low deriving (Eq, Show)
type AdjMap = M.Map String [String]
type ModMap = M.Map String Char
type FlopMap = M.Map String Bool
type ConMap = M.Map String (M.Map String Pulse)
type Signal = (String, String, Pulse)

  -- createAndStoreDotGraph edges modMap

main :: IO ()
main = do
  inputLines <- fmap lines getContents
  let (adjMap, modMap) = foldl processAdjs (M.empty, M.empty) inputLines
  let flopMap = M.fromList $ map (\(n,_) -> (n, False)) $ filter (\(_, c) -> c == '%') $ M.toList modMap
  let edges = concatMap (\(n, ns) -> [(n, n') | n' <- ns]) $ M.toList adjMap
  let conMap = M.fromList $ map (\(n,_) -> (n, M.fromList $ map (\e -> (fst e, Low)) $ filter (\(n1,n2) -> n2 == n) edges )) $ filter (\(_, c) -> c == '&') $ M.toList modMap
  let signals = third $ last $ take 1001 $ iterate (run adjMap [("button", "broadcaster", Low)]) (flopMap, conMap, [])
  print $ length (filter (==Low) $ map third signals) * length (filter (==High) $ map third signals)

third :: (a,b,c) -> c
third (_,_,c) = c

run :: AdjMap -> [Signal] -> (FlopMap, ConMap, [Signal]) -> (FlopMap, ConMap, [Signal])
run _ [] r = r
run adjs ((n1, n2, p):ss) (flops, cons, sigs) = run adjs (ss ++ nexts) (flops', cons', (n1, n2, p):sigs) where
  (nexts, flops', cons')
    | M.member n2 flops && p == High = ([], flops, cons)
    | M.member n2 flops && p == Low =
      if M.findWithDefault False n2 flops
        -- flop was on
        then (map (n2,, Low) $ M.findWithDefault [] n2 adjs, M.insert n2 False flops, cons)
        -- flop was off
        else (map (n2,, High) $ M.findWithDefault [] n2 adjs, M.insert n2 True flops, cons)
    | M.member n2 cons =
      if all (==High) $ M.elems n2Con'
        then (map (n2,,Low) $ M.findWithDefault [] n2 adjs, flops, M.insert n2 n2Con' cons)
        else (map (n2,,High) $ M.findWithDefault [] n2 adjs, flops, M.insert n2 n2Con' cons)
    | otherwise = (map (n2,,p) $ M.findWithDefault [] n2 adjs, flops, cons)
      where
        n2Con = M.findWithDefault M.empty n2 cons
        n2Con' = M.insert n1 p n2Con

processAdjs :: (AdjMap, ModMap) -> String -> (AdjMap, ModMap)
processAdjs (adjMap, modMap) str = (M.insert n ns adjMap, M.insert n t modMap)  where
  [nStr, nsStr] = splitOn " -> " str
  (n, t) = if nStr == "broadcaster" then (nStr, 'B') else (tail nStr, head nStr)
  ns = splitOn ", " nsStr