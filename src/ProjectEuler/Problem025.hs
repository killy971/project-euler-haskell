module ProjectEuler.Problem025 (solution025) where

import Util
import Data.List

digitsCount :: Show a => a -> Int
digitsCount = length . show

hasThousandDigits :: Show a => a -> Bool
hasThousandDigits = (== 1000) . digitsCount

solution025 :: Int
solution025 = head $ findIndices hasThousandDigits fibs
