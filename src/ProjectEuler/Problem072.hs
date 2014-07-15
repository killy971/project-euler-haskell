module ProjectEuler.Problem072 (solution072) where

import Control.Monad
import Math.Sieve.Phi

genericSolution :: Integer -> Integer
genericSolution = sum . ap (map . phi . sieve) (enumFromTo 2)

solution072 :: Integer
solution072 = genericSolution 1e6
