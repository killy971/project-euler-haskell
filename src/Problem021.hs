module Main where

isDivisor :: Integer -> Integer -> Bool
isDivisor x d = rem x d == 0

properDivisors :: Integer -> [Integer]
properDivisors x = filter (isDivisor x) [1..x `quot` 2]

amicalNumber :: Integer -> Bool
amicalNumber a = a /= b && a == sumB
    where b = sum $ properDivisors a
          sumB = sum $ properDivisors b

genericSolution :: Integer -> Integer
genericSolution n = sum $ filter amicalNumber [1..n]

solution :: Integer
solution = genericSolution 10000

main :: IO ()
main = print solution
