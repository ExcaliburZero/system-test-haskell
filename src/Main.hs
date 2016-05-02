{-|
Module      : Main
Description : Runs the program.
Copyright   : (c) Christopher Wells, 2016
License     : MIT
Maintainer  : cwellsny@nycap.rr.com
-}
module Main where

import System.Environment (getArgs)
import System.IO (hClose, hGetContents, openFile, IOMode(ReadMode))

import Parser
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
  handle <- openFile file ReadMode
  contents <- hGetContents handle
  let sections = toSections contents
  let definitions = map toDefinition sections
  let tests = map toTest definitions
  results <- mapM runTest tests
  let resultTexts = zipWith showResults tests results
  _ <- mapM putStrLn resultTexts
  hClose handle
