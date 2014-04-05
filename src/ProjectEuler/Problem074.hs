module ProjectEuler.Problem074 (solution074) where

import Data.Digits
import Util hiding (fact)

fact :: Int -> Int
fact 0 = 1
fact 1 = 1
fact 2 = 2
fact 3 = 6
fact 4 = 24
fact 5 = 120
fact 6 = 720
fact 7 = 5040
fact 8 = 40320
fact 9 = 362880
fact _ = error "fact n where n > 9 is not needed for this problem"

digitsFactSum :: Int -> Int
digitsFactSum = sum . map fact . digits 10

dfsChain :: Int -> [Int]
dfsChain n = n : takeWhile (/= n) (tail $ iterate digitsFactSum n)

dfsChainLength :: Int -> Int
dfsChainLength = length . takeWhileUniq . dfsChain

genericSolution :: Int -> Integer
genericSolution = toInteger . length . filter (== 60) . map dfsChainLength . enumFromTo 1

solution074 :: Integer
solution074 = genericSolution 1000000
