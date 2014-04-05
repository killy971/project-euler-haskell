module Main where

import System.Exit ( exitFailure, exitSuccess )
import TestProblems
import TestUtil
import Test.HUnit

allTests :: [Test]
allTests = [testUtil, testProblems]

main :: IO Counts
main = do
    cnt <- runTestTT (test allTests)
    if errors cnt + failures cnt == 0
        then exitSuccess
        else exitFailure
