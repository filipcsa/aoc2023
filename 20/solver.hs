import qualified Data.Graph as G
import qualified Data.Map as M
import Data.List.Split (splitOn)
import Dot (createAndStoreDotGraph)

type AdjMap = M.Map String [String]
type ModMap = M.Map String Char
type StateMap = M.Map String Bool

main :: IO ()
main = do
  inputLines <- fmap lines getContents
  let (adjMap, modMap) = foldl processAdjs (M.empty, M.empty) inputLines
  let stateMap = M.fromList $ map (\(n,_) -> (n, False)) $ filter (\(_, c) -> c == '%') $ M.toList modMap
  let edges = concatMap (\(n, ns) -> [(n, n') | n' <- ns]) $ M.toList adjMap
  createAndStoreDotGraph edges modMap

processAdjs :: (AdjMap, ModMap) -> String -> (AdjMap, ModMap)
processAdjs (adjMap, modMap) str = (M.insert n ns adjMap, M.insert n t modMap)  where
  [nStr, nsStr] = splitOn " -> " str
  (n, t) = if nStr == "broadcaster" then (nStr, 'B') else (tail nStr, head nStr)
  ns = splitOn ", " nsStr