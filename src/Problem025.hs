module Main where

import Util
import Data.List

digitsCount :: Show a => a -> Int
digitsCount = length . show

hasThousandDigits :: Show a => a -> Bool
hasThousandDigits = (== 1000) . digitsCount

solution :: Int
solution = head $ findIndices hasThousandDigits fibs

main :: IO ()
main = print solution
