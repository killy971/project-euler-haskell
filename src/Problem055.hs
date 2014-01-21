module Main where

import Control.Monad
import Data.Digits
import Util

reverseInteger :: Integer -> Integer
reverseInteger = unDigits 10 . digitsRev 10

isDecPalindrome :: Integer -> Bool
isDecPalindrome = isPalindrome . digits 10

isLychrel :: Integer -> Bool
isLychrel = all (not . isDecPalindrome) . take 50 . tail . iterate next
    where next = ap (+) reverseInteger

genericSolution :: Integer -> Int
genericSolution n = length $ filter isLychrel [1..n]

solution :: Int
solution = genericSolution 10000

main :: IO ()
main = print solution
