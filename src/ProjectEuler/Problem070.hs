module ProjectEuler.Problem070 (solution070, genericSolution070) where

import Control.Monad
import Data.Digits
import Data.List
import Data.Ratio
import Math.Sieve.Phi

phiSeq :: [Integer] -> [Integer]
phiSeq = map =<< phi . sieve . last

nPhiSeq :: [Integer] -> [(Integer, Integer)]
nPhiSeq = ap zip phiSeq

equalByPermutation :: Integral a => (a, a) -> Bool
equalByPermutation (x, y) = null $ digits 10 x \\ digits 10 y

minByRatio :: Integral a => (a, a) -> (a, a) -> (a, a)
minByRatio (n1, p1) (n2, p2) = if n1 % p1 <= n2 % p2 then (n1, p1) else (n2, p2)

genericSolution070 :: Integer -> Integer
genericSolution070 = fst . foldl1 minByRatio . filter equalByPermutation . nPhiSeq . enumFromTo 2

solution070 :: Integer
solution070 = genericSolution070 10000000
