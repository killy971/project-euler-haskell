module Util where

import Data.List (delete, tails, transpose)
import Data.Ratio

ints :: (Enum t, Num t) => [t]
ints = [1..]

sq :: Integer -> Integer
sq x = x * x

fpow :: Int -> (a -> a) -> a -> a
fpow n = foldr (.) id . replicate n

ratioAdd :: Integral a => Ratio a -> Ratio a -> Ratio a
ratioAdd x y =
	let nx = numerator x in
	let ny = numerator y in
	let dx = denominator x in
	let dy = denominator y in
	(nx * dy + ny * dx) % (dx * dy)

ratioDiv :: Integral a => Ratio a -> Ratio a -> Ratio a
ratioDiv x y =
	let nx = numerator x in
	let ny = numerator y in
	let dx = denominator x in
	let dy = denominator y in
	(nx * dy) % (dx * ny)

fact :: (Enum a, Num a) => a -> a
fact = product . enumFromTo 1

fibs :: [Integer]
fibs = 0 : scanl (+) 1 fibs

interleave :: [[a]] -> [a]
interleave = concat . transpose

isPalindrome :: Eq a => [a] -> Bool
isPalindrome xs = xs == reverse xs

clump :: Int -> [a] -> [[a]]
clump n = (fpow n init)  . map (take n) . tails

takeUntil :: (a -> Bool) -> [a] -> [a]
takeUntil _ [] = []
takeUntil p (x:xs) | p x = [x] 
                   | otherwise = x : takeUntil p xs

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
