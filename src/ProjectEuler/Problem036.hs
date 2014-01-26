module ProjectEuler.Problem036 (solution036) where

import Data.Digits
import Util

isBasePalindrome :: Integer -> Integer -> Bool
isBasePalindrome = (isPalindrome .) . digits

isDecAndBinPalindrome :: Integer -> Bool
isDecAndBinPalindrome = and . sequence [isBasePalindrome 10, isBasePalindrome 2]

genericSolution :: Integer -> Integer
genericSolution n = sum $ filter isDecAndBinPalindrome [1..n]

solution036 :: Integer
solution036 = genericSolution 1000000
