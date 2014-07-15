module ProjectEuler.Problem021 (solution021) where

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

solution021 :: Integer
solution021 = genericSolution 1e4
