module Util where

import Data.List (delete, maximumBy, group, tails, transpose)
import Data.Ord
import Data.Ratio
import qualified Data.Set as Set

ints :: (Enum t, Num t) => [t]
ints = [1..]

sq :: Integer -> Integer
sq x = x * x

fpow :: Int -> (a -> a) -> a -> a
fpow n = foldr (.) id . replicate n

foldF :: [a -> a -> a] -> [a] -> a
foldF fs (x : xs) = foldl (flip id) x $ zipWith flip fs xs
foldF _ _ = error "Util.foldF: illegal arguments"

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
fact n = product [1..n]

fibs :: [Integer]
fibs = 0 : scanl (+) 1 fibs

tribs :: [Integer]
tribs = 1 : 1 : 1 : zipWith (+) tribs (tail $ zipWith (+) tribs (tail tribs))

interleave :: [[a]] -> [a]
interleave = concat . transpose

isPalindrome :: Eq a => [a] -> Bool
isPalindrome xs = xs == reverse xs

-- Splits the list into overlapping clumps of n elements
-- http://docs.factorcode.org/content/word-clump%2Cgrouping.html
clump :: Int -> [a] -> [[a]]
clump n = fpow n init  . map (take n) . tails

takeUntil :: (a -> Bool) -> [a] -> [a]
takeUntil _ [] = []
takeUntil p (x:xs) | p x = [x]
                   | otherwise = x : takeUntil p xs

-- the input list has to be sorted
uniq :: Eq a => [a] -> [a]
uniq = map head . group

takeWhileUniq :: Ord a => [a] -> [a]
takeWhileUniq [] = []
takeWhileUniq (x:xs) = x : takeWhileUniq' (Set.singleton x) xs
    where takeWhileUniq' _ [] = []
          takeWhileUniq' set (y:ys) = if Set.member y set
              then []
              else y : takeWhileUniq' (Set.insert y set) ys

indexOfMax :: (Ord a) => [a] -> Integer
indexOfMax = fst . maximumBy (comparing snd) . zip [0..]

findIndexBy :: (Ord a) => (a -> a -> Bool) -> [a] -> Integer
findIndexBy _ [] = error "Util.findIndexBy: empty list"
findIndexBy comp (x:xs) = findIndexBy' xs x 1 0
    where
        findIndexBy' [] _ _ i = i
        findIndexBy' (y:ys) z yi zi = yi `seq` if comp y z
            then findIndexBy' ys y (yi + 1) yi
            else findIndexBy' ys z (yi + 1) zi

reversedHeads :: [a] -> [[a]]
reversedHeads = scanl (flip (:)) []

combinations :: Int -> [a] -> [[a]]
combinations 0 _ = [[]]
combinations m l = [x:ys | x:xs <- tails l, ys <- combinations (m - 1) xs]

sortedPermutations :: Eq a => [a] -> [[a]]
sortedPermutations [] = [[]]
sortedPermutations xs = [ x:ys | x <- xs, ys <- sortedPermutations (delete x xs)]

sumCombinationCount :: Integer -> [Integer] -> Integer
sumCombinationCount 0 _ = 1
sumCombinationCount _ [] = 0
sumCombinationCount r (c:cs) = if r < 0
    then 0
    else sumCombinationCount (r - c) (c:cs) + sumCombinationCount r cs

-- Count integer partitions using Euler's pentagonal number theorem
countPartitions :: Int -> Integer
countPartitions = (map p' [0..] !!)
    where p' n | n == 0 = 1
               | otherwise = sum $ zipWith (*) (map (countPartitions . (n-)) (pent n)) $ cycle [1, 1, -1, -1]
           where pent x = takeWhile (<= x) $ concatMap f [1..]
                  where f k = [(m - k) `div` 2, (m + k) `div` 2]
                         where m = 3 * k * k
