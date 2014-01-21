module Main where

import Control.Monad
import Math.Sieve.Phi

genericSolution :: Integer -> Integer
genericSolution = sum . ap (map . phi . sieve) (enumFromTo 2)

solution :: Integer
solution = genericSolution 1000000

main :: IO ()
main = print solution
