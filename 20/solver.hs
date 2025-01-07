import qualified Data.Graph as G
import qualified Data.Map as M
import Data.List.Split (splitOn)
import Data.GraphViz
import Data.List (nub)
import Data.GraphViz.Attributes.Complete ( Attribute(Color), Color(RGB), toWC )
import qualified Data.Text.Lazy.IO as T
type AdjMap = M.Map String [String]
type ModMap = M.Map String Char
type StateMap = M.Map String Bool

main :: IO ()
main = do
  inputLines <- fmap lines getContents
  let (adjMap, modMap) = foldl processAdjs (M.empty, M.empty) inputLines
  let stateMap = M.fromList $ map (\(n,_) -> (n, False)) $ filter (\(_, c) -> c == '%') $ M.toList modMap
  let edges = concatMap (\(n, ns) -> [(n, n') | n' <- ns]) $ M.toList adjMap
  let dotGraph = createDotGraph edges modMap
  T.writeFile "graph.dot" (printDotGraph dotGraph)
  -- dot -Tpng graph.dot -o graph.png

processAdjs :: (AdjMap, ModMap) -> String -> (AdjMap, ModMap)
processAdjs (adjMap, modMap) str = (M.insert n ns adjMap, M.insert n t modMap)  where
  [nStr, nsStr] = splitOn " -> " str
  (n, t) = if nStr == "broadcaster" then (nStr, 'B') else (tail nStr, head nStr)
  ns = splitOn ", " nsStr

createDotGraph :: [(String, String)] -> ModMap -> DotGraph String
createDotGraph edges modMap = graphElemsToDot params nodes edges' where
  params = nonClusteredParams { 
    fmtNode = \(n, _) -> [Color [toWC (nodeColor modMap n)], style filled],
    fmtEdge = const [Color [toWC (RGB 0 0 0)]] }
  nodes = map (, []) $ nub $ concatMap (\(n1, n2) -> [n1, n2]) edges
  edges' = map (\(n1, n2) -> (n1, n2, [])) edges

nodeColor :: ModMap -> String -> Color
nodeColor modMap n = case M.findWithDefault 'X' n modMap of
  'B' -> RGB 0 0 0
  '%' -> RGB 200 200 200 
  '&' -> RGB 255 0 0
  'X' -> RGB 0 255 0
