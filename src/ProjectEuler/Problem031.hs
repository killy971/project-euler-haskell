module ProjectEuler.Problem031 (solution031) where

combinations :: Integer -> [Integer] -> Integer
combinations 0 _ = 1
combinations _ [] = 0
combinations r (c:cs) = if r < 0
    then 0
    else combinations (r - c) (c:cs) + combinations r cs

solution031 :: Integer
solution031 = combinations 200 [200, 100, 50, 20, 10, 5, 2, 1]
