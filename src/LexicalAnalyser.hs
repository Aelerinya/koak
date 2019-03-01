module LexicalAnalyser (lexicalAnalyser) where

instructions = ["def","for","while"]
binop = ["==","!=","=","+","*","-","/","<",">"]
unop = ["!","-"]
blockDelimiters = [":","(",")",";"]
types = ["int","double","void"]
allTokens = binop ++ unop ++ blockDelimiters

lexicalAnalyser file = separateTokens file

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

getIdentifier str =
  if (elem (head str) (['A'..'Z'] ++ ['a'..'z'] ++ ['0'..'9'] ++ "."))
  then (head str) : (getIdentifier (tail str))
  else []

separateTokens [] = []
separateTokens file =
  let token = (getToken file allTokens) in
    token : (separateTokens (removeBlanks (drop (length token) file)))
