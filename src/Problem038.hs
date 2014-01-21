module Main where

import Data.Digits
import Data.List

isPandigital :: [Integer] -> Bool
isPandigital = (== [1..9]) . sort

applyProperty :: Integer -> [Integer]
applyProperty = take 9 . concat . map (digits 10) . flip map [1..9] . (*)

genericSolution :: [Integer] -> Integer
genericSolution = unDigits 10 . last . filter isPandigital . map applyProperty

solution :: Integer
solution = genericSolution [1..10000]

main :: IO ()
main = print solution
