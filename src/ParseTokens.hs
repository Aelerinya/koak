module ParseTokens (parseTokens) where

instructions = ["def","for","while"]
binop = ["==","!=","=","+","*","-","/","<",">"]
unop = ["!","-"]
blockDelimiters = [":","(",")",";"]
types = ["int","double","void"]
allTokens = binop ++ unop ++ blockDelimiters

removeBlanks str = dropWhile (`elem` " \t\n") str

isStartOfString start str =
  let len = (length start) in
    (take len str) == start

getToken str []
  | (getIdentifier str) /= [] = (getIdentifier str)
  | otherwise = []
getToken str tokens =
  if (isStartOfString (head tokens) str)
  then (head tokens)
  else (getToken str (tail tokens))

getIdentifier [] = []
getIdentifier str =
  if (elem (head str) (['A'..'Z'] ++ ['a'..'z'] ++ ['0'..'9'] ++ "."))
  then (head str) : (getIdentifier (tail str))
  else []

parseTokens [] = []
parseTokens file =
  let token = (getToken file allTokens) in
    token : (parseTokens (removeBlanks (drop (length token) file)))
