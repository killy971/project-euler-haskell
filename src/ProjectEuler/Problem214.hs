module ProjectEuler.Problem214 (solution214) where

import Data.Numbers.Primes
import Math.Sieve.Phi
import Util

mPhi :: Int -> Int
mPhi = phi (sieve (4e7 :: Integer))

phiChain :: Int -> [Int]
phiChain = takeUntil (== 1) . iterate mPhi

phiChainLength :: Int -> Int
phiChainLength = length . phiChain

genericSolution :: [Int] -> Integer
genericSolution = toInteger . sum . filter (\ x -> 25 == phiChainLength x)

solution214 :: Integer
solution214 = genericSolution $ takeWhile (<= 4e7) primes
