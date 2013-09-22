module Util where

import Data.List (delete)

fact :: (Enum a, Num a) => a -> a
fact n = product [1..n]

fibs :: [Integer]
fibs = 0 : scanl (+) 1 fibs

sortedPermutations :: Eq a => [a] -> [[a]]
sortedPermutations [] = [[]]
sortedPermutations xs = [ x:ys | x <- xs, ys <- sortedPermutations (delete x xs)]
