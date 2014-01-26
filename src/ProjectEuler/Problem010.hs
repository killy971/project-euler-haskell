module ProjectEuler.Problem010 (solution010) where

import Data.Numbers.Primes

genericSolution :: Integer -> Integer
genericSolution n = sum $ takeWhile (< n) primes

solution010 :: Integer
solution010 = genericSolution 2000000
