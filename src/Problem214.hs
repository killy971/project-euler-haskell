module Main where

import Data.Numbers.Primes
import Math.Sieve.Phi
import Util

mPhi :: Integer -> Integer
mPhi = phi (sieve (40000000::Integer))

phiChain :: Integer -> [Integer]
phiChain = takeUntil (== 1) . iterate mPhi

phiChainLength :: Integer -> Integer
phiChainLength = toInteger . length . phiChain

genericSolution :: [Integer] -> Integer
genericSolution = sum . filter (\x -> 25 == phiChainLength x)

solution :: Integer
solution = genericSolution $ takeWhile (<= 40000000) primes

main :: IO ()
main = print solution
