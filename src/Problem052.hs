module Main where

import Data.Digits
import Data.List
import Util

allEqual :: Eq a => [a] -> Bool
allEqual [] = True
allEqual (x:xs) = all (== x) xs

sameDigits :: Integer -> Bool
sameDigits = allEqual . map (sort . digits 10) . flip map [2..6] . (*)

solution :: Integer
solution = (head . filter sameDigits) ints

main :: IO ()
main = do print solution
