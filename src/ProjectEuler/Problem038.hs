module ProjectEuler.Problem038 (solution038) where

import Data.Digits
import Data.List

isPandigital :: [Integer] -> Bool
isPandigital = (== [1..9]) . sort

applyProperty :: Integer -> [Integer]
applyProperty = take 9 . concatMap (digits 10) . flip map [1..9] . (*)

genericSolution :: [Integer] -> Integer
genericSolution = unDigits 10 . last . filter isPandigital . map applyProperty

solution038 :: Integer
solution038 = genericSolution [1..10000]
