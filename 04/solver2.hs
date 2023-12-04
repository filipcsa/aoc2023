import Data.Set (Set, fromList, member, toList)
import Data.List.Split (splitOn)
import qualified Data.Map
import Data.Map (Map)
import Data.Maybe (fromJust)

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

countCards :: [Int] -> [Card] -> Int
countCards hits originalCards = totalIds where
  originalIds = map cardId originalCards
  numCardsById = Data.Map.fromList $ map (\c -> (cardId c, 1 :: Int)) originalCards
  cardCountMap = foldl (processCardWithId hits) numCardsById originalIds
  totalIds = sum $ map snd (Data.Map.toList cardCountMap)


processCardWithId :: [Int] -> Map Int Int -> Int -> Map Int Int
processCardWithId hits numCardsById cardId = updatedMap where
  numH = hits !! (cardId-1)
  numCopies = fromJust $ Data.Map.lookup cardId numCardsById
  updatedMap = foldl (increaseNumsInMap numCopies) numCardsById [cardId+1..cardId+numH]

increaseNumsInMap :: Int -> Map Int Int -> Int -> Map Int Int
increaseNumsInMap addNum origMap cardId = newMap where
  currNum = fromJust $ Data.Map.lookup cardId origMap
  newNum = currNum + addNum
  newMap = Data.Map.insert cardId newNum origMap


main :: IO ()
main = do
  contents <- getContents
  let inputLines = lines contents
  let cards = map parseCard inputLines
  let cardHits = map numHits cards
  print $ countCards cardHits cards