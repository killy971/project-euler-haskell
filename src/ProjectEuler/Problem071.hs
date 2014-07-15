module ProjectEuler.Problem071 (solution071) where

import Data.Ratio

genericSolution :: Integral a => Ratio a -> a -> a
genericSolution target maxDen = loop initNum initDen 0
    where initNum = numerator target
          initDen = 1 + denominator target
          loop num den closest
              | den == maxDen = numerator closest
              | (nextNum % den) < target = loop nextNum den (nextNum % den)
              | otherwise = loop num (den + 1) closest
                  where nextNum = num + 1

solution071 :: Integer
solution071 = genericSolution (3 % 7) 1e6
