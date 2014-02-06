module ProjectEuler.Problem076 (solution076) where

import Math.Combinat.Partitions

solution076 :: Integer
solution076 = countPartitions 100 - 1
