module ProjectEuler.Problem076 (solution076, genericSolution076) where

import Util

genericSolution076 :: Int -> Integer
genericSolution076 n = countPartitions n - 1

solution076 :: Integer
solution076 = genericSolution076 100
