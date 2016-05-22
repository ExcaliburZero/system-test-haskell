{-|
Module      : Main
Description : Runs the program.
Copyright   : (c) Christopher Wells, 2016
License     : MIT
Maintainer  : cwellsny@nycap.rr.com
-}
module Test.SystemTest.Main where

import Control.Monad (zipWithM_)
import Data.Aeson
import Data.Text()
import qualified Data.ByteString.Lazy as B (readFile)
import System.Console.ANSI
import System.Environment (getArgs)
import System.Exit (exitFailure, exitSuccess)

import Test.SystemTest.Test

{-|
  Gets the files to be tested and runs the tests they contain.
-}
progMain :: IO ()
progMain = do
  args <- getArgs
  _ <- mapM testFile args
  return ()

{-|
  Runs the tests contained in the given file, and prints out their results.
-}
testFile :: String -> IO ()
testFile file = do
  let contents = B.readFile file
  parsed <- (eitherDecode <$> contents) :: IO (Either String [Test])
  case parsed of
    Left err -> do
      putStrLn ("Unable to parse test definition file `" ++ file ++  "`:\n")
      putStrLn err
      exitFailure
    Right tests -> do
      results <- mapM runTest tests
      zipWithM_ printResults tests results
      let total = Prelude.length results
      let failures = Prelude.length $ Prelude.filter (\ (x, _) -> not x) results
      endProgram failures total

{-|
  Prints out the results of the given test with a color based on the results.
  Passing tests are printed in green, and failing tests are printed in red.
-}
printResults :: Test -> TestResults -> IO ()
printResults test results = do
  case results of
    (True, _) -> setSGR [SetColor Foreground Dull Green]
    (False, _) -> setSGR [SetColor Foreground Dull Red]
  putStrLn $ showResults test results
  setSGR [Reset]

{-|
  Handles the ending of the program. Prints out the number of tests that failed,
  if any. Also gives an exit failure if any tests failed.
-}
endProgram :: Int -> Int -> IO ()
endProgram failures total = case failures of
  0 -> do
    putStrLn "\nAll tests passed"
    exitSuccess
  _ -> do
    putStrLn ("\n" ++ show failures ++ " / " ++ show total ++ " tests failed")
    exitFailure
