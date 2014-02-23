module ProjectEuler.Problem004 (solution004) where

import Data.Digits
import Data.List
import Util

solution004 :: Integer
solution004 = last $ filter (isPalindrome . digits 10) $ sort [ x * y | x <- [100..999], y <- [x..999]]
