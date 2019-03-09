module ParseExpression (parseExpression) where

import TreeData
import Identifier
import Decimal
import Type
import Double

parseExpression :: [String] -> [Tree]
parseExpression tokens =
  [prioritizeBinops (separateBinops tokens) 4]

priorities = [("=",0),
              ("==",1),("!=",1),
              ("<",2),(">",2),
              ("+",3),("-",3),
              ("*",4),("/",4)]

prioritizeBinops :: [Tree] -> Int -> Tree
prioritizeBinops tree (-1) = (head tree)
prioritizeBinops tree priority =
  prioritizeBinops (groupAllBinops tree priority) (priority - 1)

groupAllBinops :: [Tree] -> Int -> [Tree]
groupAllBinops tree currentPriority =
  if ((length tree) == 1)
  then tree
  else let (Node binop _) = (tree !! 1) in
    let (Just priority) = lookup binop priorities in
      if (priority == currentPriority)
      then let (group,rest) = groupBinopSequence tree currentPriority in
        if (null rest)
        then [group]
        else group : (head rest) : groupAllBinops (tail rest) currentPriority
      else (take 2 tree) ++ (groupAllBinops (drop 2 tree) currentPriority)

groupBinopSequence :: [Tree] -> Int -> (Tree,[Tree])
groupBinopSequence tree currentPriority =
  if ((length tree) == 1)
  then ((head tree),[])
  else let (Node binop _) = (tree !! 1) in
    let (Just priority) = lookup binop priorities in
      if (priority == currentPriority)
      then let (next,rest) = groupBinopSequence (drop 2 tree) currentPriority in
        (Node binop [(head tree),next],rest)
      else ((head tree),(tail tree))

separateBinops :: [String] -> [Tree]
separateBinops tokens =
  let (unary,rest) = (getUnary tokens) in
    if (null rest)
    then [unary]
    else let (binop,rest2) = getBinaryOperation rest in
      unary : binop : (separateBinops rest2)

binops = ["*","/","+","-","<",">","==","!=","="]
getBinaryOperation :: [String] -> (Tree, [String])
getBinaryOperation tokens =
  if (elem (head tokens) binops)
  then (Node (head tokens) [], (tail tokens))
  else errorWithoutStackTrace ("Expected binary operator before " ++ (show tokens))

getUnary :: [String] -> (Tree, [String])
getUnary tokens =
  if (elem (head tokens) ["-","!"])
  then if ((length tokens) > 1)
       then let (unary,rest) = (getUnary (tail tokens)) in
              (Node (head tokens) [unary], rest)
       else errorWithoutStackTrace ("Missing tokens after " ++ (head tokens))
  else getPostfix tokens

getPostfix :: [String] -> (Tree, [String])
getPostfix tokens =
  let (primary,rest) = getPrimary tokens in
    if ((null rest) || (head rest) /= "(")
    then (primary, rest)
    else let (callExpressions,rest2) = getCallExpressions (tail rest) in
      (Node "!call" [Node "primary" [primary], Node "call_expr" callExpressions], rest2)

getCallExpressions :: [String] -> ([Tree], [String])
getCallExpressions tokens =
  let (p,xs) = (break (`elem` [")",","]) tokens) in
    if (null xs)
    then errorWithoutStackTrace ("Expected ')' after " ++ (show p))
    else if (head xs == ")")
         then ([Node "expr" (parseExpression p)], (tail xs))
         else let (next,rest) = getCallExpressions (tail xs) in
                ((Node "expr" (parseExpression p) : next), rest)

getPrimary :: [String] -> (Tree, [String])
getPrimary tokens
  | ((head tokens) == "(") =
      (let (p,xs) = (break (== ")") (tail tokens)) in
         if (null xs)
         then errorWithoutStackTrace ("Expected ')' in " ++ (show tokens))
         else (Node "expr" (parseExpression p), (tail xs)))
  | (isIdentifier (head tokens)) = (Node (getIdentifier tokens) [], tail tokens)
  | (isDecimal (head tokens)) = (Node (getDecimal tokens) [], tail tokens)
  | (isDouble (head tokens)) = (Node (getDouble tokens) [], tail tokens)
  | otherwise = errorWithoutStackTrace ("Unrecognized primary: " ++ (show tokens))
