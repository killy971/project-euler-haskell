module Main where

genericSolution :: Integer -> Integer
genericSolution = sum . filter multipleOf3or5 . enumFromTo 1 . subtract 1

solution :: Integer
solution = genericSolution 1000

main :: IO ()
main = print solution
