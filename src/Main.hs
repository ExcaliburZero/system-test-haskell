{-|
Module      : Main
Description : Runs the program.
Copyright   : (c) Christopher Wells, 2016
License     : MIT
Maintainer  : cwellsny@nycap.rr.com
-}
module Main where

import Control.Applicative ((<$>))
import Control.Monad (zipWithM_)
import Data.Aeson
import Data.Text
import qualified Data.ByteString.Lazy as B (readFile)
import System.Console.ANSI
import System.Environment (getArgs)
import System.IO (hClose, hGetContents, openFile, IOMode(ReadMode))

import Test

{-|
  Gets the files to be tested and runs the tests they contain.
-}
main :: IO ()
main = do
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
    Left err -> putStrLn err
    Right tests -> do
      results <- mapM runTest tests
      zipWithM_ printResults tests results
      return ()

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
