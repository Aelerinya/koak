module Decimal where

isDecimal :: String -> Bool
isDecimal [] = False
isDecimal str =
  if (elem (head str) ['0'..'9'])
  then (if (null (tail str))
    then True
    else isDecimal (tail str))
  else False

getDecimal :: [String] -> String
getDecimal tokens =
  let n = head tokens in
    if ((null n) || (not (isDecimal n)))
    then errorWithoutStackTrace "Invalid decimal constant " ++ n
    else n
