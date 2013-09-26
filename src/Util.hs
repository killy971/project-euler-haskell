module Util where

import Data.List (delete)

ints :: (Enum t, Num t) => [t]
ints = [1..]

sq :: Integer -> Integer
sq x = x * x

fact :: (Enum a, Num a) => a -> a
fact = product . enumFromTo 1

fibs :: [Integer]
fibs = 0 : scanl (+) 1 fibs

findIndexBy :: (Ord a) => (a -> a -> Bool) -> [a] -> Integer
findIndexBy _ [] = error "Util.findIndexBy: empty list"
findIndexBy f (x:xs) = findIndexBy' xs x 1 0
	where 
		findIndexBy' [] _ _ i = i
		findIndexBy' (y:ys) z yi zi = yi `seq` if f y z
			then findIndexBy' ys y (yi + 1) yi
			else findIndexBy' ys z (yi + 1) zi

sortedPermutations :: Eq a => [a] -> [[a]]
sortedPermutations [] = [[]]
sortedPermutations xs = [ x:ys | x <- xs, ys <- sortedPermutations (delete x xs)]
