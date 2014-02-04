module ProjectEuler.Problem214 (solution214) where

import Data.Numbers.Primes
import Math.Sieve.Phi
import Util

mPhi :: Integer -> Integer
mPhi = phi (sieve (40000000 :: Integer))

phiChain :: Integer -> [Integer]
phiChain = takeUntil (== 1) . iterate mPhi

phiChainLength :: Integer -> Integer
phiChainLength = toInteger . length . phiChain

genericSolution :: [Integer] -> Integer
genericSolution = sum . filter (\ x -> 25 == phiChainLength x)

solution214 :: Integer
solution214 = genericSolution $ takeWhile (<= 40000000) primes
