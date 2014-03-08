module TestUtil (testUtil) where

import Util
import Test.HUnit

testUtil :: Test
testUtil = "Util" ~: [
    map sq [0..4] ~=? [0,1,4,9,16],
	take 8 fibs ~=? [0,1,1,2,3,5,8,13],
	take 8 tribs ~=? [1,1,1,3,5,9,17,30]]
