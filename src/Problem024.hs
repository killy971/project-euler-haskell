module Main where

import Data.List

solution :: String
solution = sort (permutations "0123456789") !! (1000000 - 1)

main :: IO ()
main = do print solution
