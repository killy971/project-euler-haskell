module ProjectEuler.Problem093 (solution093) where

import Data.Digits hiding (digits)
import Data.List
import Data.Ord
import Util

isInt :: RealFrac a => a -> Bool
isInt x = x == fromInteger (round x)

foldF2 :: ([a -> a -> a], [a]) -> a
foldF2 (f1 : f2 : fs, x1 : x2 : xs) = f1 (foldF [f2] [x1, x2]) (foldF fs xs)
foldF2 _ = error "foldF2: illegal argument"

fourDigitCombs :: [[Double]]
fourDigitCombs = [[a, b, c, d] | a <- [0..9], b <- [a+1..9], c <- [b+1..9], d <- [c+1..9]]

opsCombs :: [[Double -> Double -> Double]]
opsCombs = [[x, y, z] | x <- ops, y <- ops, z <- ops]
    where ops = [(+), (-), (*), (/)]

filterNaturalCandidates :: [Double] -> [Int]
filterNaturalCandidates = map floor . sort . nub . filter isInt . filter (>= 1)

matchingNaturalsCount :: [Int] -> Int
matchingNaturalsCount = length . takeWhile (uncurry (==)) . zip [1..]

generatedNaturals :: [Double] -> Int
generatedNaturals = matchingNaturalsCount . filterNaturalCandidates . generate
    where generate digits = generate1 digits ++ generate2 digits
          generate1 = map (uncurry foldF) . allCombs
          generate2 = map foldF2 . allCombs
          allCombs digits = [(o, d) | o <- opsCombs, d <- permutations digits]

solution093 :: Integer
solution093 = convert $ maximumBy (comparing generatedNaturals) fourDigitCombs
    where convert = unDigits 10 . map floor
