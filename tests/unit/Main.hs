{-|
Module      : Main
Description : Contains unit tests of the program.
Copyright   : (c) Christopher Wells, 2016
License     : MIT
Maintainer  : cwellsny@ncap.rr.com
-}
module Main where

import Test.HUnit

import SystemTest.Test

-- | Runs the unit tests of the program
main :: IO Counts
main = do
  let tests = TestList [TestLabel "showResults" testShowResults]
  runTestTT tests

testShowResults :: Test.HUnit.Test
testShowResults = TestCase (assertEqual "test" expected actual)
  where expected = "GoodbyeTest: Failed\n  Expected: Hello, World!\n  Actual:   Goodbye, World!"
        actual   = showResults testDef result
        testDef  = Test "GoodbyeTest" "echo 'Goodbye, World!'" "Hello, World!"
        result   = (False, "Goodbye, World!")
