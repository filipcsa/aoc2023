import Data.List (nub, maximumBy)
import Data.Map ( Map, empty, lookup, insert, keys, elems, assocs, delete )
import Prelude hiding (lookup)
import Data.List.Split (splitOn)
import Data.Ord (comparing)

data Card = J | N2 | N3 | N4 | N5 | N6 | N7 | N8 | N9
  | T | Q | K | A
  deriving (Show, Eq, Ord)

data HandType =
  HighCard |
  OnePair |
  TwoPairs |
  ThreeKind |
  FullHouse |
  FourKind |
  FiveKind
  deriving (Show, Eq, Ord)

data Hand = Hand {
  cards :: [Card],
  bid :: Int,
  handType :: HandType
} deriving Show

type Occurances = Map Card Int

parseHand :: String -> Hand
parseHand str = Hand { cards=cards, bid=bid, handType=handType } where
  [cardsStr, bidStr] = splitOn " " str
  cards = map parseCard cardsStr
  bid = read bidStr
  handType = assessType cards

parseCard :: Char -> Card
parseCard 'A' = A
parseCard 'K' = K
parseCard 'Q' = Q
parseCard 'J' = J
parseCard 'T' = T
parseCard '9' = N9
parseCard '8' = N8
parseCard '7' = N7
parseCard '6' = N6
parseCard '5' = N5
parseCard '4' = N4
parseCard '3' = N3
parseCard '2' = N2

assessType :: [Card] -> HandType
assessType cards = handType where
  occurances = joker $ computeOccurances cards
  handType = assessOccurancesType occurances

computeOccurances :: [Card] -> Occurances
computeOccurances = foldl updateOccurances empty

updateOccurances :: Occurances -> Card -> Occurances
updateOccurances occurances card = updatedOccurances where
  cardOccurancesBefore = lookup card occurances
  updatedOccurances = case cardOccurancesBefore of
    Nothing -> insert card 1 occurances
    Just n -> insert card (n+1) occurances

assessOccurancesType :: Occurances -> HandType
assessOccurancesType occurances
  | numOccurances == 1 = FiveKind
  | numOccurances == 2 && maxOccurs == 4 = FourKind
  | numOccurances == 2 && maxOccurs == 3 = FullHouse
  | numOccurances == 3 && maxOccurs == 3 = ThreeKind
  | numOccurances == 3 && maxOccurs == 2 = TwoPairs
  | numOccurances == 4 = OnePair
  | numOccurances == 5 = HighCard 
  where
      numOccurances = length occurances
      maxOccurs = maximum (elems occurances)

joker :: Occurances -> Occurances
joker occurances = updatedOccurances where
  numJokers = lookup J occurances
  jokerlessOccurances = delete J occurances
  (card, numOccs) = maximumBy (comparing snd) (assocs jokerlessOccurances) 
  updatedOccurances = case numJokers of 
    Nothing -> occurances
    Just n -> if n < 5 
      then insert card (numOccs + n) jokerlessOccurances
      else occurances

isSmaller :: Hand -> Hand -> Bool
isSmaller left right = isLeftSmaller where
  leftType = handType left
  rightType = handType right
  sameType = leftType == rightType
  isLeftSmaller = if sameType
    then rightHasHigherCard (cards left) (cards right)
    else leftType < rightType

rightHasHigherCard :: [Card] -> [Card] -> Bool
rightHasHigherCard (l:ls) (r:rs) = if l == r
  then rightHasHigherCard ls rs
  else l < r

totalWinnings :: [Hand] -> Int
totalWinnings hands = sum $ zipWith (*) (map bid hands) [1..]

main :: IO ()
main = do
  contents <- getContents
  let inputLines = lines contents
  let hands = map parseHand inputLines
  let sortedHands = qsort isSmaller hands
  print $ totalWinnings sortedHands

-- qsort with comparator which returns true if first out of two is smaller
qsort :: (a -> a -> Bool) -> [a] -> [a]
qsort _ [] = []
qsort isSmaller (x:xs) = qsort isSmaller smaller ++ [x] ++ qsort isSmaller larger where
  smaller = [ s | s <- xs, isSmaller s x ]
  larger = [ l | l <- xs, not $ isSmaller l x ]