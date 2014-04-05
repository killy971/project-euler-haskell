module ProjectEuler.Problem073 (solution073) where

endNumerator :: Int -> Int
endNumerator d = if 2 * n < d
   then n
   else n - 1
      where n = quot d 2

countForDenominator :: Int -> Int
countForDenominator d = countForDenominator' (1 + quot d 3) 0
    where countForDenominator' n count = if n > endNum
             then count
             else countForDenominator' (n + 1) (if 1 == gcd d n then count + 1 else count)
          endNum = endNumerator d

genericSolution :: Int -> Integer
genericSolution = toInteger . sum . map countForDenominator . enumFromTo 4

solution073 :: Integer
solution073 = genericSolution 12000
