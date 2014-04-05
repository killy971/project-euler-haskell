module ProjectEuler.Problem203 (solution203) where

import Data.List
import Data.Numbers.Primes
import Util

pascal :: [[Integer]]
pascal = iterate (map sum . clump 2 . (:) 0 . flip (++) [0]) [1]

squareFree :: Integer -> Bool
squareFree = all (== 1) . map length . group . primeFactors

genericSolution203 :: Int -> Integer
genericSolution203 = sum . filter squareFree . nub . concat . flip take pascal

solution203 :: Integer
solution203 = genericSolution203 51
