import Data.Char (isNumber)

parseDigits :: String -> String
parseDigits []      = []
parseDigits (c:cs)  = if isNumber c
  then c : parseDigits cs
  else parseDigits cs

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