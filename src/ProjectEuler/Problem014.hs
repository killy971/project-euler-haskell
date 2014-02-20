module ProjectEuler.Problem014 (solution014, genericSolution014) where

import Util

next :: Int -> Int
next n = if odd n
    then 3 * n + 1
    else quot n 2

collatzLength :: Int -> Int
collatzLength start = collatzLength' start 2
    where collatzLength' n result = if n == 1
           then result
           else collatzLength' (next n) (result + 1)

genericSolution014 :: Int -> Integer
genericSolution014 = toInteger . (+ 1) . findIndexBy (>) . map collatzLength . enumFromTo 1

solution014 :: Integer
solution014 = genericSolution014 1000000
