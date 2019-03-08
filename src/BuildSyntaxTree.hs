module BuildSyntaxTree (buildSyntaxTree) where

--import Debug.Trace
import TreeData
import Identifier
import Decimal
import Type
import ParseExpression

buildSyntaxTree :: [String] -> Tree
buildSyntaxTree tokens =
  Node "kdefs" (getKdefs tokens)

getKdefs :: [String] -> [Tree]
getKdefs tokens =
  let (p,xs) = (break (== ";") tokens) in
    if (null xs)
    then (if (not (null p))
      then errorWithoutStackTrace ("Expected ; after " ++ (show p))
      else [])
    else (if (null p)
      then getKdefs (tail xs)
      else (separateKdefs p) : getKdefs (tail xs))

separateKdefs :: [String] -> Tree
separateKdefs [] = Empty
separateKdefs tokens
  | ((head tokens) == "def") = Node "defs" (parseDef (tail tokens))
  | otherwise = Node "expressions" (separateExpressions tokens)

parseDef :: [String] -> [Tree]
parseDef tokens =
  let (p,xs) = getPrototype tokens in
    [p,Node "expressions" (separateExpressions xs)]

getPrototype :: [String] -> (Tree, [String])
getPrototype tokens =
  let (start,rest) = getPrototypeStart tokens in
    let (args,rest2) = getPrototypeArguments rest in
      let (endType,rest3) = getPrototypeType rest2 in
        (Node "prototype" [start, args, endType], rest3)

getPrototypeStart :: [String] -> (Tree, [String])
getPrototypeStart tokens =
  let id = head tokens in
    if (id == "unary" || id == "binary")
    then let next = tokens !! 1 in
      (Node id (if (isDecimal next)
                then [Node next []] else []), (if (isDecimal next)
                                               then drop 2 tokens
                                               else tail tokens))
    else (Node "identifier" [Node (getIdentifier tokens) []], (tail tokens))

getPrototypeArguments :: [String] -> (Tree, [String])
--getPrototypeArguments tokens | trace (show tokens) False = undefined
getPrototypeArguments tokens =
  if ((null tokens) || (head tokens) /= "(")
  then errorWithoutStackTrace "Missing '(' in argument declaration"
  else let (p,xs) = break (== ")") (tail tokens) in
    (Node "prototype_args" (getArgument p), (tail xs))

getArgument :: [String] -> [Tree]
getArgument [] = []
getArgument tokens =
  if (length tokens >= 3 &&
       isIdentifier (head tokens) &&
       (tokens !! 1) == ":" &&
       isType (tokens !! 2))
  then Node "arg" [Node "identifier" [Node (head tokens) []],
                   Node "type" [Node (tokens !! 2) []]] : getArgument (drop 3 tokens)
  else errorWithoutStackTrace ("Invalid arguments syntax: " ++ (show tokens))

getPrototypeType :: [String] -> (Tree, [String])
getPrototypeType tokens =
  if ((length tokens) >= 2 &&
       (head tokens) == ":" &&
       isType (tokens !! 1))
  then (Node "type" [Node (tokens !! 1) []], (drop 2 tokens))
  else errorWithoutStackTrace ("Missing type declaration at end of def: " ++ (show tokens))

-- verifyToken :: [String] -> String -> Bool
-- verifyToken tokensList token =
--   if (null tokenList || (head tokenList) /= token)
--   then errorWithoutStackTrace "Missing '" ++ token ++ "' before " ++ tokenList
--   else (head tokenList)

separateExpressions :: [String] -> [Tree]
separateExpressions tokens
  | (head tokens) == "for" = getFor tokens
  | (head tokens) == "if" = getIf tokens
  | (head tokens) == "while" = getWhile tokens
  | otherwise = getExpression tokens

getWhile :: [String] -> [Tree]
getWhile tokens =
  let (condition,rest) = break (== "do") (tail tokens) in
    if (null condition || (length rest) < 2 || (head rest) /= "do")
    then errorWithoutStackTrace ("Syntax error in while .. do .. : " ++ (show tokens))
    else [Node "while" [Node "expr" (parseExpression condition)],
          Node "do" [Node "expressions" (separateExpressions (tail rest))]]

getFor :: [String] -> [Tree]
getFor tokens =
  let identifier = (tokens !! 1) in
    if ((not (isIdentifier identifier)) || ((tokens !! 2) /= "="))
    then errorWithoutStackTrace ("Invalid initialization in for: " ++ (identifier) ++ (tokens !! 2))
    else let (initialization,r1) = break (== ",") (drop 3 tokens) in
      if (null r1)
      then errorWithoutStackTrace("Invalid syntax in for: Missing ',' after initialization")
      else let identifier2 = (r1 !! 1) in
        if ((identifier /= identifier2) || ((r1 !! 2) /= "<"))
        then errorWithoutStackTrace ("Invalid condition in for: " ++ identifier2 ++ (r1 !! 2))
        else let (condition,r2) = break (== ",") (drop 3 r1) in
          if (null r2)
          then errorWithoutStackTrace ("Missing ',' after " ++ (show condition))
          else let (increment,r3) = break (== "in") (tail r2) in
            if (null r3)
            then errorWithoutStackTrace ("Missing 'in' after " ++ (show increment))
            else [Node "for"
                  [Node "init"
                   [Node "expr"
                    [Node "="
                     (Node identifier [] : parseExpression initialization)]],
                   Node "condition"
                    [Node "expr"
                     [Node "<"
                      (Node identifier2 [] : parseExpression condition)]],
                   Node "increment"
                    [Node "expr" (parseExpression increment)],
                   Node "expressions" (separateExpressions (tail r3))]]

getIf :: [String] -> [Tree]
getIf tokens =
  let (condition,r1) = break (== "then") (tail tokens) in
    if (null r1)
    then errorWithoutStackTrace ("Missing then in if statement")
    else let (then_expressions,r2) = break (== "else") (tail r1) in
      [Node "if"
       ((Node "condition"
         [Node "expr" (parseExpression condition)]) :
        (Node "then"
         [Node "expressions" (separateExpressions then_expressions)]) :
        if (not (null r2)) then [Node "else" (separateExpressions (tail r2))] else [])]

getExpression :: [String] -> [Tree]
getExpression tokens =
  let (p,xs) = (break (== ":") tokens) in
    let next = if (null xs) then [] else getExpression (tail xs) in
      if (null p)
      then next
      else (Node "expr" (parseExpression p)) : next
