{-|
Module      : Test
Description : Defines the Test datatype.
Copyright   : (c) Christopher Wells, 2016
License     : MIT
Maintainer  : cwellsny@nycap.rr.com
-}
module Test where

import Control.Applicative ((<$>), (<*>))
import Control.Monad (mzero)
import Data.Aeson
import Data.Text
import System.IO (hGetContents)
import System.Process (CreateProcess(..), createProcess, shell, StdStream(CreatePipe), waitForProcess)

-- | Represents a single test, with a name, a command, and an expected result.
data Test = Test
  { name           :: String -- ^ The name of the Test.
  , command        :: String -- ^ The command the to be run for the Test.
  , expectedOutput :: String -- ^ The expected result of the output of the command.
  }

-- | Shows the contents of the values of the Test.
instance Show Test where
  show test = "Test {name = " ++ show (name test) ++ ", command = " ++ show (command test) ++ ", expectedOutput = " ++ show (expectedOutput test) ++ "}"

-- | Converts JSON into Test.
instance FromJSON Test where
  parseJSON (Object v) = Test
    <$> v .: pack "name"
    <*> v .: pack "command"
    <*> v .: pack "expectedOutput"
  parseJSON _ = mzero

-- | Represents the results of a single test, including if it passed or failed,
-- and the actual output.
type TestResults = (Bool, String)

{-|
  Runs the command of a Test and checks to see if the output of the command is
  the same as the expected output of the Test.

  If the result of running the command is equal to the expected result, then
  True will be returned. However, if the expected and actual results are not
  equal, then False will be returned.

  The actual output of the test is also returned, so that it can be printed out
  in the case where the test fails.

  >>> runTest (Test "Hello" "echo 'Hello, World!'" "Hello, World!")
  (True,"Hello, World!")

  >>> runTest (Test "Hello" "echo 'Goodbye, World!'" "Hello, World!")
  (False,"Goodbye, World!")
-}
runTest :: Test -> IO TestResults
runTest (Test _ testCommand expected) = do
  (_, Just stdout, _, handler) <- createProcess (shell testCommand){ std_out = CreatePipe }
  _ <- waitForProcess handler
  actualNL <- hGetContents stdout
  let actual = Prelude.init actualNL
  let result = expected == actual
  return (result, actual)

{-|
  Returns a String showing the results of the Test. Includes the name of the
  Test and if it passed or failed.

  >>> let test = Test "HelloTest" "echo 'Hello, World!'" "Hello, World!"
  >>> result <- runTest test
  >>> showResults test result
  "HelloTest: Passed"

  >>> let test = Test "GoodbyeTest" "echo 'Goodbye, World!'" "Hello, World!"
  >>> result <- runTest test
  >>> showResults test result
  "GoodbyeTest: Failed\n  Expected: Hello, World!\n  Actual:   Goodbye, World!"
-}
showResults :: Test -> TestResults -> String
showResults (Test testName _ _) (True, _) = testName ++ ": Passed"
showResults (Test testName _ expected) (False, actual) = testName ++ ": Failed\n  Expected: " ++ expected ++ "\n  Actual:   " ++ actual
