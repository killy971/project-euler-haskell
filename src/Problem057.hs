module Main where

import Data.Digits
import Data.Ratio
import Util

next :: Ratio Integer -> Ratio Integer
next = (+ 1) . (ratioDiv 1) . (+ 1)

piExpansions :: Int -> [Ratio Integer]
piExpansions = tail . flip take (iterate next 1)

numeratorHasMoreDigitsThanDenominator :: Ratio Integer -> Bool
numeratorHasMoreDigitsThanDenominator x =
    (length $ digits 10 $ numerator x) > (length $ digits 10 $ denominator x)

genericSolution :: Int -> Int
genericSolution = length . filter numeratorHasMoreDigitsThanDenominator . piExpansions

solution :: Int
solution = genericSolution 1000

main :: IO ()
main = print solution
