import qualified Data.Graph as G
import qualified Data.Map as M
import Data.List.Split (splitOn)

type AdjMap = M.Map String [String]
type ModMap = M.Map String Char

main :: IO ()
main = do
  inputLines <- fmap lines getContents
  let (adjMap, modMap) = foldl processAdjs (M.empty, M.empty) inputLines
  let edges = concatMap (\(n, ns) -> [(n, n') | n' <- ns]) $ M.toList adjMap
  print (adjMap, modMap)
  print edges

processAdjs :: (AdjMap, ModMap) -> String -> (AdjMap, ModMap)
processAdjs (adjMap, modMap) str = (M.insert n ns adjMap, M.insert n t modMap)  where
  [nStr, nsStr] = splitOn " -> " str
  (n, t) = if nStr == "broadcaster" then (nStr, 'B') else (tail nStr, head nStr)
  ns = splitOn ", " nsStr
