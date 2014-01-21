module Main where

import Data.Digits
import Data.Function.Memoize
import Util
import qualified Data.Set as Set

takeWhileUniq :: Ord a => [a] -> [a]
takeWhileUniq [] = []
takeWhileUniq (x:xs) = x : takeWhileUniq' (Set.singleton x) xs
    where takeWhileUniq' _ [] = []
          takeWhileUniq' set (y:ys) = if Set.member y set
              then []
              else y : takeWhileUniq' (Set.insert y set) ys

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

genericSolution :: Integer -> Int
genericSolution = length . filter (== 60) . map dfsChainLength . enumFromTo 1

solution :: Int
solution = genericSolution 1000000

main :: IO ()
main = print solution
