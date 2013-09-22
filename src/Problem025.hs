module Main where

import Util
import Data.List

digitsCount :: Show a => a -> Int
digitsCount x = length $ show x

hasThousandDigits :: Show a => a -> Bool
hasThousandDigits x = digitsCount x == 1000

solution :: Int
solution = case findIndex hasThousandDigits fibs of
	Just value -> value
	Nothing    -> -1

main :: IO ()
main = do print solution
