module Main where

import Data.Digits
import Util

digitsSquareSum :: Integer -> Integer
digitsSquareSum = sum . map sq . digits 10

chainEndsWith :: Integer -> Integer
chainEndsWith x =
	let next = digitsSquareSum x in
		if next == 1 || next == 89
			then next
			else chainEndsWith next

genericSolution :: Integer-> Int
genericSolution = length . filter (== 89) . map chainEndsWith . enumFromTo 1

solution :: Int
solution = genericSolution 10000000

main :: IO ()
main = print solution
