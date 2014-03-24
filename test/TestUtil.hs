{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}

module TestUtil (testUtil) where

import Util
import Test.HUnit

testSq = "sq" ~: map sq [0..4] @?= [0, 1, 4, 9, 16]

testFpow = "fpow" ~: [
    fpow 5 (^2) 2 @?= 2 ^ 2 ^ 5,
    fpow 2 (++ "+") "i" @?= "i++"]

testFoldF = "foldF" ~: [
    foldF [(+), (*), (-)] [2, 3, 5, 7] @?= 18]

testFact = "fact" ~: [map fact [0..7] @?= [1, 1, 2, 6, 24, 120, 720, 5040]]

testFibs = "fibs" ~: take 8 fibs @?= [0, 1, 1, 2, 3, 5, 8, 13]

testTribs = "tribs" ~: take 8 tribs @?= [1, 1, 1, 3, 5, 9, 17, 31]

testInterleave = "interleave" ~: [
    -- interleave [] ~=? [],
    interleave [[2]] @?= [2],
    interleave [[2, 3]] @?= [2, 3],
    interleave [[2], [3]] @?= [2, 3],
    interleave [[2], []] @?= [2],
    interleave [[], [2]] @?= [2],
    interleave [[2, 5], [3]] @?= [2, 3, 5],
    interleave [[2], [3, 5]] @?= [2, 3, 5],
    interleave [[2, 5], [3, 7]] @?= [2, 3, 5, 7],
    interleave [[2, 5, 7], [3]] @?= [2, 3, 5, 7],
    interleave [[2, 7, 17], [3, 11, 19], [5, 13, 23]] @?= [2, 3, 5, 7, 11, 13, 17, 19, 23]]

testIsPalindrome = "isPalindrome" ~: [
    isPalindrome "" @?= True,
    isPalindrome "a" @?= True,
    isPalindrome "aba" @?= True,
    isPalindrome "abba" @?= True,
    isPalindrome "abcba" @?= True,
    isPalindrome "ab" @?= False,
    isPalindrome "abb" @?= False,
    isPalindrome "abc" @?= False]

testClump = "clump"~: [
    -- clump 0 [] @?= []]
    clump 1 [2] @?= [[2]],
    clump 1 [2, 3] @?= [[2], [3]],
    clump 2 [2] @?= [],
    clump 2 [2, 3] @?= [[2, 3]],
    clump 2 [2, 3, 5] @?= [[2, 3], [3, 5]],
    clump 2 [2, 3, 5, 7] @?= [[2, 3], [3, 5], [5, 7]],
    clump 3 [2, 3] @?= [],
    clump 3 [2, 3, 5] @?= [[2, 3, 5]],
    clump 3 [2, 3, 5, 7] @?= [[2, 3, 5], [3, 5, 7]],
    clump 3 [2, 3, 5, 7, 11] @?= [[2, 3, 5], [3, 5, 7], [5, 7, 11]]]

testUtil = "Util" ~: [
    testSq,
    testFpow,
    testFoldF,
    testFact,
    testFibs,
    testTribs,
    testInterleave,
    testIsPalindrome,
    testClump]
