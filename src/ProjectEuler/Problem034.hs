module ProjectEuler.Problem034 (solution034) where

import Control.Monad
import Data.Digits
import Data.Function.Memoize
import Util

mFact :: Integer -> Integer
mFact = memoize fact

equalToSumOfDigitsFactorials :: Integer -> Bool
equalToSumOfDigitsFactorials = ap (==) (sum . map mFact . digits 10)

genericSolution :: Integer -> Integer
genericSolution = sum . filter equalToSumOfDigitsFactorials . enumFromTo 10

solution034 :: Integer
solution034 = genericSolution 1e5
