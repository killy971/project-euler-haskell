module Main where

import Data.Numbers.Primes

genericSolution :: Integer -> Integer
genericSolution n = sum $ takeWhile (< n) primes

solution :: Integer
solution = genericSolution 2000000

main :: IO ()
main = print solution
