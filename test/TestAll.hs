module Main where

import Test.HUnit
import Util

tests :: Test
tests = "All" ~: [
    -- 233168 ~=? solution001
    4 ~=? sq 2,
    5040 ~=? fact 7,
	[0, 1, 1, 2, 3, 5, 8, 13] ~=? take 5 fibs]

main = do runTestTT tests
