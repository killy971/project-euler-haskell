-- http://en.wikipedia.org/wiki/Methods_of_computing_square_roots

module ProjectEuler.Problem064 (solution064) where

import Math.NumberTheory.Powers
import Util

next :: (Integer, Integer, Integer, Integer) -> (Integer, Integer, Integer, Integer)
next (m, d, a, s) = (nextM, nextD, nextA, s)
    where nextM = d * a - m
          nextD = quot (s - (nextM * nextM)) d
          nextA = quot (integerSquareRoot s + nextM) nextD

initTuple :: Integer -> (Integer, Integer, Integer, Integer)
initTuple s = (0, 1, integerSquareRoot s, s)

fracPeriod :: Integer -> Int
fracPeriod = subtract 1 . length . takeWhileUniq . iterate next . initTuple

genericSolution064 :: Integer -> Int
genericSolution064 = length . filter odd . map fracPeriod . filter (not . isSquare) . enumFromTo 2

solution064 :: Integer
solution064 = toInteger $ genericSolution064 1e4
