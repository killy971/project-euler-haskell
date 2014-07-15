module ProjectEuler.Problem092 (solution092, genericSolution092) where

import Data.Digits
import Util

digitsSquareSum :: Integer -> Integer
digitsSquareSum = sum . map sq . digits 10

chainEndsWith :: Integer -> Integer
chainEndsWith x =
    let next = digitsSquareSum x in
        if next == 1 || next == 89
            then next
            else chainEndsWith next

genericSolution092 :: Integer -> Integer
genericSolution092 = toInteger . length . filter (== 89) . map chainEndsWith . enumFromTo 1

solution092 :: Integer
solution092 = genericSolution092 1e7
