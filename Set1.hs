module Main where

import Test.HUnit

test1 = TestCase (assertEqual "gound zero" 1 1)

tests = TestList [TestLabel "test1" test1]

main = runTestTTAndExit tests
