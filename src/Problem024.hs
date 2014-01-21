module Main where

import Util

solution :: String
solution = sortedPermutations "0123456789" !! (1000000 - 1)

main :: IO ()
main = putStrLn solution
