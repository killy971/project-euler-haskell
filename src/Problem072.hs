module Main where

import Math.Sieve.Phi

genericSolution :: Integer -> Integer
genericSolution n = sum $ map (phi $ sieve n) [2..n]

solution :: Integer
solution = genericSolution 1000000

main :: IO ()
main = do print solution
