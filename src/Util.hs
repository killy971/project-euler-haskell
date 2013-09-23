module Util where

import Data.List (delete)

sq :: Integer -> Integer
sq x = x * x

fact :: (Enum a, Num a) => a -> a
fact n = product [1..n]

fibs :: [Integer]
fibs = 0 : scanl (+) 1 fibs

findIndexBy :: (Ord a) => (a -> a -> Bool) -> [a] -> Integer
findIndexBy _ [] = error "Util.findIndexBy: empty list"
findIndexBy f (x:xs) = findIndexBy' xs x 1 0
	where 
		findIndexBy' [] _ _ i = i
		findIndexBy' (x:xs) y xi yi = xi `seq` if f x y
			then findIndexBy' xs x (xi + 1) xi
			else findIndexBy' xs y (xi + 1) yi

sortedPermutations :: Eq a => [a] -> [[a]]
sortedPermutations [] = [[]]
sortedPermutations xs = [ x:ys | x <- xs, ys <- sortedPermutations (delete x xs)]
