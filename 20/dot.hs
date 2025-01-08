module Dot (createAndStoreDotGraph) where 

import qualified Data.Map as M
import qualified Data.Text.Lazy.IO as T
import Data.GraphViz
import Data.GraphViz.Attributes.Complete ( Attribute(Color), Color(RGB), toWC )
import Data.List (nub)

createAndStoreDotGraph :: [(String, String)] -> M.Map String Char -> IO ()
createAndStoreDotGraph edges modMap = do
  let dotGraph = createDotGraph edges modMap
  T.writeFile "graph.dot" (printDotGraph dotGraph)
  -- dot -Tpng graph.dot -o graph.png

createDotGraph :: [(String, String)] -> M.Map String Char -> DotGraph String
createDotGraph edges modMap = graphElemsToDot params nodes edges' where
  params = nonClusteredParams { 
    fmtNode = \(n, _) -> [Color [toWC (nodeColor modMap n)], style filled],
    fmtEdge = const [Color [toWC (RGB 0 0 0)]] }
  nodes = map (, []) $ nub $ concatMap (\(n1, n2) -> [n1, n2]) edges
  edges' = map (\(n1, n2) -> (n1, n2, [])) edges

nodeColor :: M.Map String Char -> String -> Color
nodeColor modMap n = case M.findWithDefault 'X' n modMap of
  'B' -> RGB 0 0 0
  '%' -> RGB 200 200 200 
  '&' -> RGB 255 0 0
  'X' -> RGB 0 255 0
