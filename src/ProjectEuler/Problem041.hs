module ProjectEuler.Problem041 (solution041) where

import Data.Digits
import Data.List
import Data.Numbers.Primes

pandigital :: Integer -> Bool
pandigital x =
    let d = digits 10 $ fromIntegral x
    in sort d == [1..(length d)]

-- no need to include leading 9 and 8 as they cannot form prime numbers
solution041 :: Integer
solution041 = last . filter pandigital . takeWhile (<= 7654321) $ primes
