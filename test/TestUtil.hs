{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}

module TestUtil (testUtil) where

import Util
import Test.HUnit

testSq = "sq" ~: map sq [0..4] @?= [0, 1, 4, 9, 16]

testFpow = "fpow" ~: [
    fpow 5 (^2) 2 @?= 2 ^ 2 ^ 5,
    fpow 2 (++ "+") "i" @?= "i++",
    fpow 0 (++ "+") "i" @?= "i"]

testFoldF = "foldF" ~: [
    foldF [] [2] @?= 2,
    foldF [(+), (*), (-)] [2, 3, 5, 7] @?= 18]

testFact = "fact" ~: [map fact [0..7] @?= [1, 1, 2, 6, 24, 120, 720, 5040]]

testFibs = "fibs" ~: take 8 fibs @?= [0, 1, 1, 2, 3, 5, 8, 13]

testTribs = "tribs" ~: take 8 tribs @?= [1, 1, 1, 3, 5, 9, 17, 31]

testInterleave = "interleave" ~: [
    interleave [] @?= ([] :: [Int]),
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

testClump = "clump" ~: [
    clump 1 [] @?= ([] :: [[Int]]),
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

testTakeUntil = "takeUntil" ~: [
    takeUntil undefined [] @?= ([] :: [Int]),
    takeUntil odd [2] @?= [2],
    takeUntil odd [2, 3] @?= [2, 3],
    takeUntil odd [2, 3, 5] @?= [2, 3],
    takeUntil even [2] @?= [2],
    takeUntil even [2, 3] @?= [2],
    takeUntil even [2, 3, 5] @?= [2]]

testUniq = "uniq" ~: [
    uniq [] @?= ([] :: [Int]),
    uniq [2] @?= [2],
    uniq [2, 3] @?= [2, 3],
    uniq [2, 2, 3] @?= [2, 3],
    uniq [2, 3, 3] @?= [2, 3],
    uniq [2, 2, 3, 3] @?= [2, 3],
    uniq [2, 3, 2] @?= [2, 3, 2],
    uniq [2, 2, 3, 2] @?= [2, 3, 2],
    uniq [2, 3, 2, 2] @?= [2, 3, 2],
    uniq [2, 3, 3, 2] @?= [2, 3, 2],
    uniq [2, 2, 3, 3, 3, 5, 5, 5, 5, 5] @?= [2, 3, 5]]

testTakeWhileUniq = "takeWhileUniq" ~: [
    takeWhileUniq [] @?= ([] :: [Int]),
    takeWhileUniq [2, 2] @?= [2],
    takeWhileUniq [2, 3] @?= [2, 3],
    takeWhileUniq [3, 3] @?= [3],
    takeWhileUniq [2, 3, 2] @?= [2, 3],
    takeWhileUniq [3, 2, 3] @?= [3, 2],
    takeWhileUniq [2, 3, 5, 2] @?= [2, 3, 5],
    takeWhileUniq [2, 3, 5, 3] @?= [2, 3, 5],
    takeWhileUniq [2, 3, 5, 5] @?= [2, 3, 5]]

testIndexOfMax = "indexOfMax" ~: [
    indexOfMax [2] @?= 0,
    indexOfMax [2, 3] @?= 1,
    indexOfMax [3, 2] @?= 0,
    indexOfMax [2, 3, 5] @?= 2,
    indexOfMax [2, 5, 3] @?= 1,
    indexOfMax [5, 2, 3] @?= 0,
    indexOfMax [5, 3, 2] @?= 0,
    indexOfMax [3, 2, 5] @?= 2,
    indexOfMax [3, 5, 2] @?= 1]

testFindIndexBy = "findIndexBy" ~: [
    findIndexBy (>) [2] @=? 0,
    findIndexBy (<) [2] @=? 0,
    findIndexBy (<) [2, 3] @=? 0,
    findIndexBy (<) [3, 2] @=? 1,
    findIndexBy (<) [2, 3, 5] @=? 0,
    findIndexBy (<) [2, 5, 3] @=? 0,
    findIndexBy (<) [3, 2, 5] @=? 1,
    findIndexBy (<) [5, 2, 3] @=? 1,
    findIndexBy (<) [3, 5, 2] @=? 2,
    findIndexBy (<) [5, 3, 2] @=? 2]

testReversedHeads = "reversedHeads" ~: [
    reversedHeads [] @?= ([[]] :: [[Int]]),
    reversedHeads [2, 3, 5, 7, 11] @?= [[], [2], [3, 2], [5, 3, 2], [7, 5, 3, 2], [11, 7, 5, 3, 2]]]

testCombinations = "combinations" ~: [
    combinations 0 [] @?= ([[]] :: [[Int]]),
    combinations 1 [] @?= ([] :: [[Int]]),
    combinations 0 [2, 3, 5] @?= [[]],
    combinations 1 [2, 3, 5] @?= [[2], [3], [5]],
    combinations 2 [2, 3, 5] @?= [[2, 3], [2, 5], [3, 5]],
    combinations 3 [2, 3, 5] @?= [[2, 3, 5]],
    combinations 4 [2, 3, 5] @?= []]

testCountPartitions = "countPartitions" ~: [
    map countPartitions [0..9] @?= [1,1,2,3,5,7,11,15,22,30]]

testUtil = "Util" ~: [
    testSq,
    testFpow,
    testFoldF,
    testFact,
    testFibs,
    testTribs,
    testInterleave,
    testIsPalindrome,
    testClump,
    testTakeUntil,
    testUniq,
    testTakeWhileUniq,
    testIndexOfMax,
    testFindIndexBy,
    testReversedHeads,
    testCombinations,
    testCountPartitions]
