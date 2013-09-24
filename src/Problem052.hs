module Main where

import Data.Digits
import Data.List

allEqual :: Eq a => [a] -> Bool
allEqual [] = True
allEqual (x:xs) = all (== x) xs

sameDigits :: Integer -> Bool
sameDigits x = allEqual $ map (sort . digits 10) (map (* x) [2..6])

solution :: Integer
solution = head $ filter sameDigits [1..]

main :: IO ()
main = do print solution
