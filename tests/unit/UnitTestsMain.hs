{-|
Module      : Main
Description : Contains unit tests of the program.
Copyright   : (c) Christopher Wells, 2016
License     : MIT
Maintainer  : cwellsny@ncap.rr.com
-}
module Main where

import Test.HUnit

import Test.SystemTest.Test

-- | Runs the unit tests of the program
main :: IO Counts
main = do
  let tests = TestList [ TestLabel "showResults Pass" testShowResultsPass
                       , TestLabel "showResults Fail" testShowResultsFail
                       ]
  runTestTT tests

-- | A case of the showResults function where a passing test is given.
testShowResultsPass :: Test.HUnit.Test
testShowResultsPass = TestCase (assertEqual "showResults Pass" expected actual)
  where expected = "HelloTest: Passed"
        actual   = showResults testDef result
        testDef  = Test "HelloTest" "echo 'Hello, World!'" "Hello, World!"
        result   = (True, "Hello, World!")

-- | A case of the showResults function where a failing test is given.
testShowResultsFail :: Test.HUnit.Test
testShowResultsFail = TestCase (assertEqual "showResults Fail" expected actual)
  where expected = "GoodbyeTest: Failed\n  Expected: Hello, World!\n  Actual:   Goodbye, World!"
        actual   = showResults testDef result
        testDef  = Test "GoodbyeTest" "echo 'Goodbye, World!'" "Hello, World!"
        result   = (False, "Goodbye, World!")
