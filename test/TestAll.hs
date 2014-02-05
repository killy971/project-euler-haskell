module Main where

import ProjectEuler.Problem001
import ProjectEuler.Problem002
import ProjectEuler.Problem009
import ProjectEuler.Problem010
import ProjectEuler.Problem012
import ProjectEuler.Problem014
import ProjectEuler.Problem021
import ProjectEuler.Problem024
import ProjectEuler.Problem025
import ProjectEuler.Problem031
import ProjectEuler.Problem033
import ProjectEuler.Problem034
import ProjectEuler.Problem036
import ProjectEuler.Problem038
import ProjectEuler.Problem041
import ProjectEuler.Problem043
import ProjectEuler.Problem052
import ProjectEuler.Problem055
import ProjectEuler.Problem057
import ProjectEuler.Problem064
import ProjectEuler.Problem065
import ProjectEuler.Problem070
import ProjectEuler.Problem071
import ProjectEuler.Problem072
import ProjectEuler.Problem073
import ProjectEuler.Problem074
import ProjectEuler.Problem077
import ProjectEuler.Problem092
import ProjectEuler.Problem214
import System.Exit ( exitFailure, exitSuccess )
import Test.HUnit

tests :: Test
tests = "All" ~: [
    solution001 ~=? 233168,
    solution002 ~=? 4613732,
    solution009 ~=? 31875000,
    solution010 ~=? 142913828922,
    solution012 ~=? 76576500,
    solution014 ~=? 837799,
    solution021 ~=? 31626,
    solution024 ~=? 2783915460,
    solution025 ~=? 4782,
    solution031 ~=? 73682,
    solution033 ~=? 100,
    solution034 ~=? 40730,
    solution036 ~=? 872187,
    solution038 ~=? 932718654,
    solution041 ~=? 7652413,
    solution043 ~=? 16695334890,
    solution052 ~=? 142857,
    solution055 ~=? 249,
    solution057 ~=? 153,
    solution064 ~=? 1322,
    solution065 ~=? 272,
    solution070 ~=? 8319823,
    solution071 ~=? 428570,
    solution072 ~=? 303963552391,
    solution073 ~=? 7295372,
    solution074 ~=? 402,
    solution077 ~=? 71,
    solution092 ~=? 8581146,
    solution214 ~=? 1677366278943]
    -- 4 ~=? sq 2,
    -- 5040 ~=? fact 7,
	-- [0, 1, 1, 2, 3, 5, 8, 13] ~=? take 8 fibs]

main :: IO Counts
main = do
    cnt <- runTestTT (test [tests])
    if errors cnt + failures cnt == 0
        then exitSuccess
        else exitFailure
