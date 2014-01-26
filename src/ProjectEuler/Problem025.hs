module ProjectEuler.Problem025 (solution025) where

import Data.List
import Util

digitsCount :: Show a => a -> Int
digitsCount = length . show

hasThousandDigits :: Show a => a -> Bool
hasThousandDigits = (== 1000) . digitsCount

solution025 :: Integer
solution025 = toInteger $ head $ findIndices hasThousandDigits fibs
