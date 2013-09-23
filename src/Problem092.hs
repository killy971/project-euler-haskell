module Main where

import Data.Digits
import Util

digitsSquareSum :: Integer -> Integer
digitsSquareSum x = sum $ map sq (digits 10 x)

chainEndsWith :: Integer -> Integer
chainEndsWith x =
	let next = digitsSquareSum x in
		if next == 1 || next == 89
			then next
			else chainEndsWith next

genericSolution :: Integer-> Int
genericSolution limit = length $ filter (== 89) (map chainEndsWith [1..limit])

solution :: Int
solution = genericSolution 10000000

main :: IO ()
main = do print solution
