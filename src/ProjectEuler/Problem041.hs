module ProjectEuler.Problem041 (solution041) where

import Data.Digits
import Data.List
import Data.Numbers.Primes

pandigital :: Int -> Bool
pandigital x =
    let d = digits 10 x
    in sort d == [1..(length d)]

intPrimes :: [Int]
intPrimes = map fromIntegral primes

-- no need to include leading 9 and 8 as they cannot form prime numbers
solution041 :: Int
solution041 = last . filter pandigital . takeWhile (<= 7654321) $ intPrimes
