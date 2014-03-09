module ProjectEuler.Problem225 (solution225) where

import Data.List

takeUntilSubSeq :: Eq a => [a] -> [a] -> [a]
takeUntilSubSeq _ [] = []
takeUntilSubSeq ss (x : xs) = if ss `isPrefixOf` xs
    then [x]
    else x : takeUntilSubSeq ss xs

modTribs :: Integer -> [Integer]
modTribs n = modTribs' 1 1 1
    where modTribs' x y z = x : modTribs' y z ((x + y + z) `mod` n)

nonTribDivisor :: Integer -> Bool
nonTribDivisor = notElem 0 . takeUntilSubSeq [1, 1, 1] . modTribs

genericSolution225 :: Int -> Integer
genericSolution225 = (filter nonTribDivisor [1,3..] !!) . subtract 1

solution225 :: Integer
solution225 = genericSolution225 124
