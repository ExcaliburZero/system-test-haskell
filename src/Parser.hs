{-|
Module      : Parser
Description : Defines functions for parsing and creating Tests from files.
Copyright   : (c) Christopher Wells, 2016
License     : MIT
Maintainer  : cwellsny@ncap.rr.com
-}
module Parser where

import Data.List.Split (splitOn)

import Test

-- | A String representing the section of a test definition file which contains a single test definition.
type TestSection = String

-- | A String list representing a definition a test.
type TestDefinition = [String]

{-|
  Breaks up a tests file String into the sections for each of the defined tests.

  >>> let s = "1\n2\n3\n----\n4\n5\n6\n----\n1\n2\n3\n"
  >>> toSections s
  ["1\n2\n3\n","\n4\n5\n6\n","\n1\n2\n3\n"]

  >>> let s = "\n\n1\n2\n\n3\n----\n\n4\n5\n6\n"
  >>> toSections s
  ["\n\n1\n2\n\n3\n","\n\n4\n5\n6\n"]
-}
toSections :: String -> [TestSection]
toSections contents = splitOn "----" contents

{-|
  Converts a test section into the corresponding test definition.

  >>> let sec = "Name\nCommand\nExpected\n"
  >>> toDefinition sec
  ["Name","Command","Expected"]

  >>> let sec = "\nName\n\nCommand\nExpected\n\n\n"
  >>> toDefinition sec
  ["Name","Command","Expected"]

-}
toDefinition :: TestSection -> TestDefinition
toDefinition section = filter (\x -> x /= "") $ lines section

{-|
  Converts a test definition into the corresponding Test.

  >>> let def = ["Name","Command","Expected"]
  >>> toTest def
  Test {name = "Name", command = "Command", expectedOutput = "Expected"}
-}
toTest :: TestDefinition -> Test
toTest definition = Test name command expected
  where name = head definition
        command = definition !! 1
        expected = definition !! 2
