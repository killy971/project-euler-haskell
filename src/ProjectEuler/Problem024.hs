module ProjectEuler.Problem024 (solution024) where

import Data.Digits
import Util

solution024 :: Integer
solution024 = unDigits 10 $ sortedPermutations [0..9] !! (1e6 - 1)
