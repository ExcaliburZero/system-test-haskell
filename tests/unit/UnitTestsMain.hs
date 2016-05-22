{-|
Module      : Main
Description : Contains unit tests of the program.
Copyright   : (c) Christopher Wells, 2016
License     : MIT
Maintainer  : cwellsny@ncap.rr.com
-}
module Main where

import Test.HUnit
import Data.Aeson
import Data.ByteString.Lazy.Internal

import Test.SystemTest.Test

-- | Runs the unit tests of the program
main :: IO Counts
main = do
  let tests = TestList [ TestLabel "showResults Pass" testShowResultsPass
                       , TestLabel "showResults Fail" testShowResultsFail
                       , TestLabel "parseJson Valid" testParseTestJSON
                       , TestLabel "parseJson Invalid" testParseInvalidTestJSON
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

-- | A test of the JSON parsing of a Test definition, where valid JSON is given.
testParseTestJSON :: Test.HUnit.Test
testParseTestJSON = TestCase (assertEqual "Test FromJSON" expected actual)
  where expected = show [Test.SystemTest.Test.Test {name = "name", command = "command", expectedOutput = "expectedOutput"}]
        actual   = show actualP
        actualP  = case parsed of
                     Left _  -> [Test.SystemTest.Test.Test {name = "fail", command = "fail", expectedOutput = "fail"}]
                     Right x -> x
        parsed   = (eitherDecode $ packChars contents) :: Either String [Test.SystemTest.Test.Test]
        contents = "[{\n\"name\": \"name\",\n\"command\": \"command\",\n\"expectedOutput\": \"expectedOutput\" }]"

-- | A test of the JSON parsing of a Test definition, where invalid JSON is given.
testParseInvalidTestJSON :: Test.HUnit.Test
testParseInvalidTestJSON = TestCase (assertEqual "Test FromJSON" expected actual)
  where expected = False
        actual   = case parsed of
                     Left _  -> False
                     Right _ -> True
        parsed   = (eitherDecode $ packChars contents) :: Either String [Test.SystemTest.Test.Test]
        contents = "[{\n\"name\": \"name\" }]"
