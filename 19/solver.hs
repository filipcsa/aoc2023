import Text.Regex.TDFA ( (=~), AllMatches(getAllMatches), AllTextMatches (getAllTextMatches) )
import Prelude hiding (lookup)
import Data.List.Split (splitOn)
import Data.Map (Map, fromList, insert, lookup)
import Data.Maybe (fromJust)

data Part = Part { x :: Int, m :: Int, a :: Int, s :: Int } deriving Show

data Rule = Rule {
  category :: Part -> Int,
  compOp :: Int -> Int -> Bool,
  compVal :: Int,
  targetWorkflow :: String
}

instance