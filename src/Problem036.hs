module Main where

import Data.Digits
import Util

isBasePalindrome :: Integer -> Integer -> Bool
isBasePalindrome = (isPalindrome .) . digits

isDecAndBinPalindrome :: Integer -> Bool
isDecAndBinPalindrome = and . sequence [isBasePalindrome 10, isBasePalindrome 2]

genericSolution :: Integer -> Integer
genericSolution n = sum $ filter isDecAndBinPalindrome [1..n]

solution :: Integer
solution = genericSolution 1000000

main :: IO ()
main = do print solution
