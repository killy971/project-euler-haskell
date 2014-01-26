module ProjectEuler.Problem012 (solution012) where

triangleNumbers :: [Integer]
triangleNumbers = 0 : zipWith (+) triangleNumbers [1..]

isDivisor :: Integer -> Integer -> Bool
isDivisor x d = rem x d == 0

intSqrt :: Integer -> Integer
intSqrt = floor . sqrt . fromIntegral

divisorsCount :: Integer -> Integer
divisorsCount x = fromIntegral . (* 2) . length $ filter (isDivisor x) [1..intSqrt x]

genericSolution :: Integer -> Integer
genericSolution n = head $ filter ((> n) . divisorsCount) triangleNumbers

solution012 :: Integer
solution012 = genericSolution 500
