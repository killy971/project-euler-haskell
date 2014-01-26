module ProjectEuler.Problem001 (solution001) where

multipleOf3or5 :: Integer -> Bool
multipleOf3or5 x = x `mod` 3 == 0 || x `mod` 5 == 0

genericSolution001 :: Integer -> Integer
genericSolution001 = sum . filter multipleOf3or5 . enumFromTo 1 . subtract 1

solution001 :: Integer
solution001 = genericSolution001 1000
