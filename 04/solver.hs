import Data.Set (Set, fromList, member, toList)
import Data.List.Split (splitOn)

type IntSet = Set Int
data Card = Card { cardId :: Int, winningNumbers :: IntSet, cardNumbers :: IntSet }
  deriving Show

parseCard :: String -> Card
parseCard str = Card { cardId = cId, winningNumbers = wNums, cardNumbers = cNums } where
  (cardIdStr, numsStr) = (takeWhile (/=':') str, drop 2 $ dropWhile (/=':') str)
  cId = parseCardId cardIdStr
  [wNumsStr, cNumsStr] = splitOn " | " numsStr
  wNums = parseIntSet wNumsStr
  cNums = parseIntSet cNumsStr

parseCardId :: String -> Int
parseCardId str = read $ last $ splitOn " " str

parseIntSet :: String -> IntSet
parseIntSet str = fromList nums where
  nums = map (read . lstrip) (filter (/="") $ splitOn " " str) :: [Int]

lstrip :: String -> String
lstrip = dropWhile (==' ')

computeScore :: Card -> Int
computeScore card = score where
  hits = numHits card
  score = case hits of
    0 -> 0
    n -> 2^(n-1)

numHits :: Card -> Int
numHits card = length $ filter isMember (toList (cardNumbers card)) where
  isMember n = member n (winningNumbers card)

main :: IO ()
main = do
  contents <- getContents
  let inputLines = lines contents
  let cards = map parseCard inputLines
  let cardScores = map computeScore cards
  print cardScores
  print $ sum cardScores