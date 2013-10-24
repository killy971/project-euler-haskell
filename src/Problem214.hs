module Main where

import Data.Numbers.Primes
import Math.Sieve.Phi
import Util

memoizedSieve = sieve 40000000

mPhi :: Integer -> Integer
mPhi = phi memoizedSieve

phiChain :: Integer -> [Integer]
phiChain = takeUntil (== 1) . iterate mPhi

phiChainLength :: Integer -> Integer
phiChainLength = toInteger . length . phiChain

genericSolution :: [Integer] -> Integer
genericSolution = sum . filter (\x -> 25 == phiChainLength x)

solution :: Integer
solution = genericSolution $ takeWhile (<= 40000000) primes

main :: IO ()
main = do print solution
