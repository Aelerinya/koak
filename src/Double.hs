module Double where

import Decimal

isDouble :: String -> Bool
isDouble str
  | isDecimal str = True
  | (head str) == '.' = isDecimal (tail str)
  | otherwise = let (p,xs) = break (== '.') str in
      if ((null xs) ||
          (not (isDecimal p)) ||
          ((not (null (tail xs))) && (not (isDecimal (tail xs)))))
      then False
      else True

getDouble :: [String] -> String
getDouble tokens =
  if (isDouble (head tokens))
  then (head tokens)
  else errorWithoutStackTrace ("Expected double const before " ++ (show tokens))
