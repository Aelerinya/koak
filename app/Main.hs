module Main where

import System.Environment
import ParseTokens

--main :: IO ()
main = do
  args <- getArgs
  file <- readFile (args !! 0)
  putStrLn (show (parseTokens file))
