module ProjectEuler.Problem043 (solution043) where

import Data.Digits
import Util

p :: [Integer]
p = [2, 3, 5, 7, 11, 13, 17]

toInt :: [Integer] -> Integer
toInt = unDigits 10

isDivisor :: Integral a => a -> a -> Bool
isDivisor d x = rem x d == 0

subStringDivisibilityProperty :: [Integer] -> Bool
subStringDivisibilityProperty = and . zipWith isDivisor p . map (unDigits 10) . drop 1 . clump 3

solution043 :: Integer
solution043 = sum $ map toInt $ filter subStringDivisibilityProperty (sortedPermutations [0..9])
