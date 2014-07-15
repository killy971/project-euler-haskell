module ProjectEuler.Problem085 (solution085) where

import Data.List
import Data.Ord

f :: (Int, Int) -> Int
f (x, y) = x * (x + 1) * y * (y + 1)

diff :: Int -> Int -> Int
diff x y = abs $ x - y

tuples :: Int -> [(Int, Int)]
tuples limit = [(x, y) | x <- [1..limit], y <- [x..limit]]

solution085 :: Integer
solution085 =  toInteger $ uncurry (*) $ minimumBy (comparing $ diff 8e6 . f) $ tuples 2000
