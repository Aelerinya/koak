module Identifier where

getIdentifier :: [String] -> String
getIdentifier tokens =
  let id = head tokens in
    if ((null id) || (not (isIdentifier id)))
    then errorWithoutStackTrace "Invalid identifier " ++ id
    else id

isIdentifier :: String -> Bool
isIdentifier [] = False
isIdentifier str =
  if (elem (head str) (['a'..'z'] ++ ['A'..'Z']))
  then (if (null (tail str))
    then True
    else isIdentifierNext (tail str))
  else False

isIdentifierNext :: String -> Bool
isIdentifierNext [] = False
isIdentifierNext str =
  if (elem (head str) (['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9']))
  then (if (null (tail str))
    then True
    else isIdentifierNext (tail str))
  else False
