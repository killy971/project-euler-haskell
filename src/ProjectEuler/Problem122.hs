module ProjectEuler.Problem122 (solution122) where

import Data.List

minMult :: Integer -> Int
minMult x = minMult' [[1]]
    where minMult' l = case find ((== x) . head) l of
            Just result -> length result - 1
            Nothing -> minMult' $ concatMap next $ filter ((< x) . head) l
          next l = map ((: l) . (head l +)) l

genericSolution122 :: Integer -> Integer
genericSolution122 = toInteger . sum . map minMult . enumFromTo 1

solution122 :: Integer
solution122 = genericSolution122 200
