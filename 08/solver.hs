import Text.Regex.TDFA ( (=~), AllMatches(getAllMatches), AllTextMatches (getAllTextMatches) )
import qualified Data.Map as M
import Data.List.Split (splitOn)

type Trans = M.Map String (String, String)

main :: IO ()
main = do
  content <- getContents
  let inputLines = lines content
  let [movesStrs, transStrs] = splitOn [""] inputLines
  let (moves, trans) = (head movesStrs, foldl parseTrans M.empty transStrs)
  print $ countMovesToZ moves trans "AAA" 0

countMovesToZ :: String -> Trans -> String -> Int -> Int
countMovesToZ _ _ "ZZZ" n = n
countMovesToZ ms trans node n = countMovesToZ ms trans node' (n+1) where
  mv = ms !! (n `mod` length ms)
  (l, r) = M.findWithDefault (node, node) node trans
  node' = if mv == 'L' then l else r

parseTrans :: Trans -> String -> Trans
parseTrans acc str = M.insert from (l, r) acc where
  regex = "([A-Z]+) = \\(([A-Z]+), ([A-Z]+)\\)"
  (_, _, _, [from, l, r]) = str =~ regex :: (String, String, String, [String])