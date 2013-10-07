module Main where

import Data.Digits
import Data.Function.Memoize
import Util
import qualified Data.Set as Set

takeWhileUniq :: Ord a => [a] -> [a]
takeWhileUniq [] = []
takeWhileUniq (x:xs) = x : takeWhileUniq' (Set.singleton x) xs
    where takeWhileUniq' _ [] = []
          takeWhileUniq' set (x:xs) = if Set.member x set
              then []
              else x : takeWhileUniq' (Set.insert x set) xs

mFact :: Integer -> Integer
mFact = memoize fact

digitsFactSum :: Integer -> Integer
digitsFactSum = memoize sum . map mFact . digits 10

dfsChain :: Integer -> [Integer]
dfsChain n = n : takeWhile (/= n) (tail (iterate digitsFactSum n))

dfsChainLength :: Integer -> Int
dfsChainLength = length . takeWhileUniq . dfsChain

genericSolution :: Integer -> Int
genericSolution = length . filter (== 60) . map dfsChainLength . enumFromTo 1

solution :: Int
solution = genericSolution 1000000

main :: IO ()
main = do print solution
