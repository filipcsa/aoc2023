import Data.Char (isNumber)

parseDigits :: String -> String
parseDigits [] = []
parseDigits cs | isNumber (head cs) = head cs : parseDigits (tail cs)
               | otherwise = case parseStrNumAtStart cs of
                    Just strNumWithLength -> fst strNumWithLength : 
                      parseDigits (tail cs)
                    Nothing     -> parseDigits  $ tail cs

parseStrNumAtStart :: String -> Maybe (Char, Int)
parseStrNumAtStart str | take 3 str == "one" = Just ('1', 3)
                       | take 3 str == "two" = Just ('2', 3)
                       | take 5 str == "three" = Just ('3', 5)
                       | take 4 str == "four" = Just ('4', 4)
                       | take 4 str == "five" = Just ('5', 4)
                       | take 3 str == "six" = Just ('6', 3)
                       | take 5 str == "seven" = Just ('7', 5)
                       | take 5 str == "eight" = Just ('8', 5)
                       | take 4 str == "nine" = Just ('9', 4)
                       | otherwise = Nothing

getNumberInLine :: String -> Int
getNumberInLine lineOfDigits = num where
  firstDigit = head lineOfDigits
  lastDigit = last lineOfDigits
  strDigit = firstDigit : [lastDigit]
  num = read strDigit

main :: IO ()
main = do
  contents <- getContents
  let inputLines = lines contents
  let digitsInLines = map parseDigits inputLines
  let numbersPerLine = map getNumberInLine digitsInLines
  print $ sum numbersPerLine