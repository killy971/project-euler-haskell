module Main where

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
solution :: Int
solution = (last . filter pandigital . takeWhile (<= 7654321)) intPrimes

main :: IO ()
main = do print solution
