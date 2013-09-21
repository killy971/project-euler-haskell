module Main where

import Util

genericSolution :: Integer -> Integer
genericSolution n = sum (takeWhile (< n) (filter even fibs))

solution :: Integer
solution = genericSolution 4000000

main :: IO ()
main = do
	print solution
