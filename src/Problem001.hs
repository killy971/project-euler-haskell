module Main where

multipleOf3or5 :: Integer -> Bool
multipleOf3or5 x = x `mod` 3 == 0 || x `mod` 5 == 0

genericSolution :: Integer -> Integer
genericSolution = sum . filter multipleOf3or5 . enumFromTo 1 . subtract 1

solution :: Integer
solution = genericSolution 1000

main :: IO ()
main = do print solution
