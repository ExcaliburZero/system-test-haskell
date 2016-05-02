{-|
Module      : Main
Description : Runs the program.
Copyright   : (c) Christopher Wells, 2016
License     : MIT
Maintainer  : cwellsny@nycap.rr.com
-}
module Main where

import Control.Applicative ((<$>))
import Data.Aeson
import Data.Text
import qualified Data.ByteString.Lazy as B (readFile)
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
      let resultTexts = Prelude.zipWith showResults tests results
      _ <- mapM putStrLn resultTexts
      return ()
