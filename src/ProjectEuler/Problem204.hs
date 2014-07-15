module ProjectEuler.Problem204 (solution204) where

import Data.Numbers.Primes

prodCombinationCount :: Int -> [Int] -> Int
prodCombinationCount limit = prodCombinationCount' 1 1
    where prodCombinationCount' count _ [] = count
          prodCombinationCount' count p (m:ms) = if p > limit
              then count - 1
              else prodCombinationCount' (count + 1) (p * m) (m:ms)
                 + prodCombinationCount' 0 p ms

genericSolution204 :: Int -> Int -> Int
genericSolution204 hammingType limit =
    prodCombinationCount limit $ takeWhile (<= hammingType) primes

solution204 :: Integer
solution204 = toInteger $ genericSolution204 100 1e9
