module ProjectEuler.Problem024 (solution024) where

import Util

solution024 :: String
solution024 = sortedPermutations "0123456789" !! (1000000 - 1)
