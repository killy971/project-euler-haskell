module ProjectEuler.Problem077 (solution077) where

import Data.Numbers.Primes
import Util

combCountExceeds :: Integer -> Integer -> Bool
combCountExceeds n x = n < sumCombinationCount x (takeWhile (<= x) primes)

genericSolution077 :: Integer -> Integer
genericSolution077 = head . flip filter [2..] . combCountExceeds

solution077 :: Integer
solution077 = genericSolution077 5000
