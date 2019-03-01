module Main where

import System.Environment
import LexicalAnalyser

--main :: IO ()
main = do
  args <- getArgs
  file <- readFile (args !! 0)
  putStrLn (show (lexicalAnalyser file))
