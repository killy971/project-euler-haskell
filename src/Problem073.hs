module Main where

startNumerator :: Integer -> Integer
startNumerator d = if n > 3 * d
    then n
    else n + 1
        where n = quot d 3

endNumerator :: Integer -> Integer
endNumerator d = if 2 * n < d
   then n
   else n - 1
      where n = quot d 2

countForDenominator :: Integer -> Integer
countForDenominator d = countForDenominator' (startNumerator d) 0
    where countForDenominator' n count = if n > endNum
             then count
             else countForDenominator' (n + 1) (if 1 == gcd d n then count + 1 else count)
          endNum = endNumerator d

genericSolution :: Integer -> Integer
genericSolution = sum . map countForDenominator . enumFromTo 4

solution :: Integer
solution = genericSolution 12000

main :: IO ()
main = print solution
