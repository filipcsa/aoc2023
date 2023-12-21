import Prelude hiding (lookup)
import Data.List (minimumBy)
import Data.Ord (comparing)

type Layout = [[Int]]
type Pos = (Int, Int)
data Node = Source | Node { pos :: Pos, prev :: Node, cost :: Int }
  deriving (Show, Eq)

parseLine :: String -> [Int]
parseLine = map (\c -> read [c])

calculateMinCostPath :: Layout -> IO Int
calculateMinCostPath layout = dijkstraCost layout targetPos queue where
  source = Node { pos=(0,0), prev=Source, cost=0 }
  targetPos = (length layout - 1, length (head layout) - 1)
  queue = [source]

dijkstraCost :: Layout -> Pos -> [Node] -> IO Int
dijkstraCost layout target queue
  | pos minCostNext == target = return $ cost minCostNext
  | otherwise = do 
    let queueWithoutMin = removeFromQueue minCostNext queue
    let neighbors = findNeighbors layout minCostNext
    let updatedQueue = queueWithoutMin ++ neighbors
    print minCostNext
    print updatedQueue
    print ""
    dijkstraCost layout target updatedQueue where
      minCostNext = findMinCost queue

findMinCost :: [Node] -> Node
findMinCost = minimumBy (comparing cost)

removeFromQueue :: Node -> [Node] -> [Node]
removeFromQueue toRemove [] = []
removeFromQueue toRemove (n:ns)
  | n == toRemove = ns
  | otherwise = n : removeFromQueue toRemove ns

findNeighbors :: Layout -> Node -> [Node]
findNeighbors layout node = filter (isValid layout node) neighborCandidates where
  nodeCost = cost node
  nodeX = fst $ pos node
  nodeY = snd $ pos node
  neighborCandidates =
    [ Node { pos=(x,nodeY), cost=nodeCost + (layout !! x !! nodeY), prev=node } |
    x <- [nodeX - 1, nodeX + 1]] ++
    [ Node { pos=(nodeX, y), cost=nodeCost + (layout !! nodeX !! y), prev=node } |
    y <- [nodeY - 1, nodeY + 1] ]

isValid :: Layout -> Node -> Node -> Bool
isValid layout curr next = inBounds && nextNotPrev && notFourthInSameDir where
  (x, y) = pos next
  inBounds = x >= 0 && y >= 0 && x < length layout && y < length (head layout)
  nextNotPrev = prev curr /= next
  upToLastThree = getLastInPath curr 3
  notFourthInSameDir = not $ isFourthInSameDir upToLastThree next

getLastInPath :: Node -> Int -> [Node]
getLastInPath _ 0 = []
getLastInPath Source _ = []
getLastInPath node n = node : getLastInPath (prev node) (n-1)

isFourthInSameDir :: [Node] -> Node -> Bool
isFourthInSameDir ns n = length ns == 3 && (fourXs || fourYs) where
  nx = fst $ pos n
  ny = snd $ pos n
  poss = map pos ns
  fourXs = and [ nx == fst p | p <- poss ]
  fourYs = and [ ny == snd p | p <- poss ]

main :: IO ()
main = do
  contents <- getContents
  let inputLines = lines contents
  let layout = map parseLine inputLines :: Layout
  minCostPath <- calculateMinCostPath layout
  print minCostPath