module Main where

import Util
import Data.List

digitsCount :: Show a => a -> Int
digitsCount = length . show

hasThousandDigits :: Show a => a -> Bool
hasThousandDigits = (== 1000) . digitsCount

solution :: Int
solution = case findIndex hasThousandDigits fibs of
	Just value -> value
	Nothing    -> -1

main :: IO ()
main = do print solution
