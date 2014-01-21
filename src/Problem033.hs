module Main where

import Data.Ratio

reductibleFractions :: [Ratio Integer]
reductibleFractions = [(10 * a + b) / (10 * b + c) | a <- [1..9], b <- [1..9], c <- [1..9], 9 * a * c + b * c == 10 * a * b]

solution033 :: Integer
solution033 = denominator $ product reductibleFractions

main :: IO ()
main = print solution033
