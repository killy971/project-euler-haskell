module Util where

fibs :: [Integer]
fibs = 0 : scanl (+) 1 fibs
