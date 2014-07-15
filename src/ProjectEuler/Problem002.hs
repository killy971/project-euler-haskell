module ProjectEuler.Problem002 (solution002) where

import Util

genericSolution :: Integer -> Integer
genericSolution n = sum $ takeWhile (< n) (filter even fibs)

solution002 :: Integer
solution002 = genericSolution 4e6
