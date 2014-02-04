module ProjectEuler.Problem074 (solution074) where

import Data.Digits
import Data.Function.Memoize
import Util

mFact :: Integer -> Integer
mFact = memoize fact

digitsFactSum :: Integer -> Integer
digitsFactSum = sum . map mFact . digits 10

mDigitsFactSum :: Integer -> Integer
mDigitsFactSum = memoize digitsFactSum

dfsChain :: Integer -> [Integer]
dfsChain n = n : takeWhile (/= n) (tail $ iterate mDigitsFactSum n)

dfsChainLength :: Integer -> Int
dfsChainLength = length . takeWhileUniq . dfsChain

genericSolution :: Integer -> Integer
genericSolution = toInteger . length . filter (== 60) . map dfsChainLength . enumFromTo 1

solution074 :: Integer
solution074 = genericSolution 1000000
