module ProjectEuler.Problem009 (solution009) where

genericSolution :: Integer -> Integer -> Integer -> Integer -> Integer
genericSolution a b c n
    | a * a + b * b == c * c = a * b * c
    | c < b = genericSolution (a + 1) (a + 2) (n - a - 3) n
    | otherwise = genericSolution a (b + 1) (n - a - b - 1) n

solution009 :: Integer
solution009 = genericSolution 1 2 997 1000
