module Main where

import Data.Digits
import Data.Function.Memoize
import Util

mFact :: Integer -> Integer
mFact = memoize fact

equalToSumOfDigitsFactorials :: Integer -> Bool
equalToSumOfDigitsFactorials n = n == sum (map mFact $ digits 10 n)

genericSolution :: Integer -> Integer
genericSolution limit = sum $ filter equalToSumOfDigitsFactorials [10..limit]

solution :: Integer
solution = genericSolution 100000

main :: IO ()
main = do print solution
