module Main where

genericSolution :: Integer -> Integer -> Integer -> Integer -> Integer
genericSolution a b c n =
	if a * a + b * b == c * c
		then a * b * c
		else if c < b
			then genericSolution (a + 1) (a + 2) (n - a - 3) n
			else genericSolution a (b + 1) (n - a - b - 1) n

solution :: Integer
solution = genericSolution 1 2 997 1000

main :: IO ()
main = print solution
