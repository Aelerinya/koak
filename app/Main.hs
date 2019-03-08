module Main where

import System.Environment
import ParseTokens
import BuildSyntaxTree
import TreeData

displayTree :: [Char] -> Tree -> IO ()
displayTree spacing tree =
  if (tree == Empty)
  then putStrLn (spacing ++ "empty")
  else do
    let (Node str branches) = tree
    putStrLn (spacing ++ str ++ (if (null branches ) then "" else ":"))
    mapM_ (displayTree (spacing ++ "  ")) branches

main :: IO ()
main = do
  args <- getArgs
  file <- readFile (args !! 0)
  let tokens = parseTokens file
  putStrLn (show tokens)
  let tree = buildSyntaxTree tokens
  --putStrLn (show tree)
  displayTree "" tree
