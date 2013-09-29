module Main where

import Data.Ratio

genericSolution :: Integral a => Ratio a -> a -> a
genericSolution target maxDen = loop initNum initDen 0
    where initNum = numerator target
          initDen = 1 + (denominator target)
          loop num den closest = if den == maxDen
              then numerator closest
              else if (nextNum % den) < target
                  then loop nextNum den (nextNum % den)
                  else loop num (den + 1) closest
                  where nextNum = num + 1

solution :: Integer
solution = genericSolution (3 % 7) 1000000

main :: IO ()
main = do print solution
