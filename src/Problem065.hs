module Main where

import Data.Digits
import Data.Ratio
import Util

eCF :: [Integer]
eCF = 2 : 1 : interleave [iterate (+ 2) 2, repeat 1, repeat 1]

eConvergent :: Int -> [Integer]
eConvergent = flip take eCF

eFracAccFunc :: Ratio Integer -> Ratio Integer -> Ratio Integer
eFracAccFunc = ratioAdd . (ratioDiv 1)

eFrac :: Int -> Ratio Integer
eFrac = foldl1 eFracAccFunc . map (% 1) . reverse . eConvergent

genericSolution :: Int -> Integer
genericSolution = sum . digits 10 . numerator . eFrac

solution :: Integer
solution = genericSolution 100

main :: IO ()
main = do print solution
