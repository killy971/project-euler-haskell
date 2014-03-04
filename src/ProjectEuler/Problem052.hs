module ProjectEuler.Problem052 (solution052) where

import Data.Digits
import Data.List
import Util

allEqual :: Eq a => [a] -> Bool
allEqual xs = and $ zipWith (==) xs (tail xs)

sameDigits :: Integer -> Bool
sameDigits = allEqual . map (sort . digits 10) . flip map [2..6] . (*)

solution052 :: Integer
solution052 = head . filter sameDigits $ ints
