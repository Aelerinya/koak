module InferTypes where

import Debug.Trace
import TreeData
import TypedTreeData
import Identifier
import Decimal
import Double

inferTypes :: Tree -> TypedTree
inferTypes tree =
  let Node ast kdefs = tree;
      (kdefsNodes,programType) = typeKdefs kdefs []
  in if (programType == "none")
     then errorWithoutStackTrace ("Last statement must have a return value")
     else TypedNode programType "program" kdefsNodes

typeKdefs :: [Tree] -> [(String, [String])] -> ([TypedTree],String)
typeKdefs [] symbols = ([],[])
--typeKdefs kdefs symbols | trace ("kdef: " ++ show symbols) False = undefined
typeKdefs kdefs symbols =
  let Node kdef content = (head kdefs) in
    let (node, newSymbols) = (if (kdef == "defs")
                              then typeDef content symbols
                              else separateTypeExpressions content symbols);
        TypedNode nodeType _ _ = node;
        (nextKdefs,programType) = typeKdefs (tail kdefs) newSymbols
    in if (null (tail kdefs))
       then ([node], nodeType)
       else ((node : nextKdefs), programType)

typeDef :: [Tree] -> [(String, [String])] -> (TypedTree, [(String, [String])])
typeDef def symbols =
  let (Node _ prototype) = (head def);
      [(Node _ [Node id _]),(Node _ args),(Node _ [Node idType _])] = prototype;
      (argsNode, argsSymbols, argList) = typeDefArgs args;
      prototypeList = if (null argList) then ["void"] else argList;
      newSymbols = if ((lookup id symbols) == Nothing)
                   then (id,idType:prototypeList) : symbols
                   else errorWithoutStackTrace ("Multiple definition of: " ++ id);
      (Node _ exprs) = (def !! 1);
      (exprsNode,_) = separateTypeExpressions exprs argsSymbols
  in (TypedNode "none" "defs"
      [TypedNode idType id [],
       TypedNode "none" "args" argsNode,
      exprsNode], newSymbols)

typeDefArgs :: [Tree] -> ([TypedTree], [(String, [String])], [String])
typeDefArgs [] = ([],[],[])
typeDefArgs args =
  let Node _ [Node _ [Node id _], Node _ [Node idType _]] = (head args);
      (nextDefArgs,symbols,prototype) = typeDefArgs (tail args) in
    if ((lookup id symbols) /= Nothing)
    then errorWithoutStackTrace ("Multiple definition of: " ++ id)
    else ((TypedNode idType id [] : nextDefArgs),
          ((id,[idType]) : symbols),
          (idType : prototype))

separateTypeExpressions :: [Tree] -> [(String, [String])] -> (TypedTree, [(String, [String])])
--separateTypeExpressions expressions symbols | trace ("sep exprs: " ++ show symbols) False = undefined
separateTypeExpressions expressions symbols =
  let Node first args = (head expressions);
      (node,newSymbols,nodeType) | first == "if" = typeIf args symbols
                                 | first == "while" = typeWhile args symbols
                                 | first == "for" = typeFor args symbols
                                 | otherwise = typeExpressions args symbols
  in (TypedNode nodeType "expressions" node, newSymbols)
typeIf :: [Tree] -> [(String, [String])] -> ([TypedTree], [(String, [String])], String)
--typeIf args symbols | trace ("if: " ++ show symbols) False = undefined
typeIf args symbols =
  let (Node _ [Node _ [condition]] : Node _ [Node _ thenArg] : maybeElse) = args;
      (conditionNode,newSymbols) = typeExpression condition symbols
      TypedNode typeCondition _ _ = conditionNode;
      (thenNode,newSymbols2) = separateTypeExpressions thenArg newSymbols;
      TypedNode typeThen _ _ = thenNode
  in if (not (null maybeElse))
     then let Node _ [Node _ elseArg] = head maybeElse;
              (elseNode,newSymbols3) = separateTypeExpressions elseArg newSymbols2;
              TypedNode typeElse _ _ = elseNode
          in ([TypedNode typeThen "if"
              [TypedNode typeCondition "condition" [conditionNode],
               TypedNode typeThen "then" [thenNode],
               TypedNode typeElse "else" [elseNode]]], newSymbols3,typeThen)
     else ([TypedNode typeThen "if"
            [TypedNode typeCondition "condition" [conditionNode],
             TypedNode typeThen "then" [thenNode]]],newSymbols2,typeThen)

typeWhile :: [Tree] -> [(String, [String])] -> ([TypedTree], [(String, [String])], String)
typeWhile args symbols =
  let [Node _ [Node _ [condition]], Node _ exprs] = args;
      (conditionNode,newSymbols) = typeExpression condition symbols;
      TypedNode conditionType _ _ = conditionNode;
      (exprsNode,newSymbols2,exprsType) = typeExpressions exprs newSymbols
  in  ([TypedNode exprsType "while"
        [TypedNode conditionType "condition" [conditionNode],
         TypedNode exprsType "expressions" exprsNode]], newSymbols2, exprsType)

typeFor :: [Tree] -> [(String, [String])] -> ([TypedTree], [(String, [String])], String)
typeFor args symbols =
  let [Node _ [Node _ [initArg]],
       Node _ [Node _ [condition]],
       Node _ [Node _ [increment]],
       Node _ exprs] = args;
      (initNode,newSymbols) = typeExpression initArg symbols;
      TypedNode initType _ _ = initNode;
      (conditionNode,newSymbols2) = typeExpression condition newSymbols;
      TypedNode conditionType _ _ = conditionNode;
      (incrementNode,newSymbols3) = typeExpression increment newSymbols2;
      TypedNode incrementType _ _ = incrementNode;
      (exprsNode,newSymbols4,exprsType) = typeExpressions exprs newSymbols3
  in ([TypedNode exprsType "for"
       [TypedNode initType "init" [initNode],
        TypedNode conditionType "condition" [conditionNode],
        TypedNode incrementType "increment" [incrementNode],
        TypedNode exprsType "expressions" exprsNode]], newSymbols4, exprsType)

typeExpressions :: [Tree] -> [(String, [String])] -> ([TypedTree], [(String, [String])], String)
typeExpressions [] symbols = ([],[],[])
--typeExpressions exprs symbols | trace ("exprs: " ++ show symbols) False = undefined
typeExpressions exprs symbols =
  let (node,newSymbols) = typeExpression (head exprs) symbols;
      TypedNode nodeType _ _ = node
  in if (null (tail exprs))
     then ([node],newSymbols,nodeType)
     else let (nextNode,newSymbols2,nextType) = typeExpressions (tail exprs) newSymbols
          in ((node : nextNode), newSymbols2, nextType)

binops = ["*","/","+","-","<",">","==","!=","="]
unops = ["-","!"]
typeExpression :: Tree -> [(String, [String])] -> (TypedTree, [(String, [String])])
--typeExpression expr symbols | trace ("expr: " ++ show symbols) False = undefined
typeExpression expr symbols =
  let Node op arg = expr in
    if (op == "!call")
    then typeCall expr symbols
    else if (op == "expr")
         then typeExpression (head arg) symbols
         else if (length arg) == 2
              then typeBinop expr symbols
              else if (elem op unops)
                   then typeUnop expr symbols
                   else typePrimary expr symbols

comparison = ["<",">","==","!="]
typeBinop :: Tree -> [(String, [String])] -> (TypedTree, [(String, [String])])
--typeBinop expr symbols | trace ("binop: " ++ show symbols ++ show expr) False = undefined
typeBinop expr symbols =
  let (Node op [arg1,arg2]) = expr;
      (node1, newSymbols1) = (if (op == "=")
                             then (TypedNode "" "" [],symbols)
                             else typeExpression arg1 symbols);
      (TypedNode type1 _ _) = node1;
      (node2, newSymbols2) = typeExpression arg2 newSymbols1;
      (TypedNode type2 _ _) = node2;
      newType = if (elem op comparison)
                then "int" else(getTypePriority type1 type2)
  in if (op == "=")
     then let (Node id _) = arg1;
              (updatedSymbols,status) = replaceSymbol id [type2] newSymbols2
          in (TypedNode type2 op
              [TypedNode type2 id [TypedNode "none" status []],node2], updatedSymbols)
     else (TypedNode newType op [node1,node2], newSymbols2)

replaceSymbol :: String -> [String] -> [(String, [String])] -> ([(String, [String])],String)
replaceSymbol key keyType [] = ([(key,keyType)],"first")
replaceSymbol key keyType symbols =
  let (k,kt) = (head symbols) in
    if (key == k)
    then ((key,keyType):(tail symbols),"not first")
    else let (nextSymbols,status) = replaceSymbol key keyType (tail symbols)
         in ((head symbols):nextSymbols,status)

getTypePriority :: String -> String -> String
getTypePriority t1 t2
  | t1 == "TypeVar" = t2
  | t1 == "int" = if (t2 == "TypeVar") then "int" else t2
  | otherwise = "double"

typeUnop :: Tree -> [(String, [String])] -> (TypedTree, [(String, [String])])
typeUnop expr symbols =
  let (Node op [arg]) = expr;
      (node,newSymbols) = typeExpression arg symbols;
      TypedNode nodeType _ _ = node in
  (TypedNode nodeType op [node], newSymbols)

typeCall :: Tree -> [(String, [String])] -> (TypedTree, [(String, [String])])
typeCall expr symbols =
  let (Node _ [Node _ [Node fun _], Node _ args]) = expr;
      maybeTypeFun = lookup fun symbols;
      (Just (typeFun:prototype)) = if (maybeTypeFun == Nothing)
                                   then errorWithoutStackTrace ("Undeclared function: " ++ fun)
                                   else maybeTypeFun;
      (argsNodes,newSymbols) = if (((head prototype) == "void") && (length args /= 0)
                                   || ((head prototype) /= "void") && ((length prototype) /= (length args)))
                               then  errorWithoutStackTrace ("Invalid number of arguments for function: " ++ fun)
                               else if ((head prototype) == "void")
                                    then ([],symbols)
                                    else typeCallArgs args symbols prototype
  in (TypedNode typeFun "!call"
      [TypedNode typeFun fun [],
       TypedNode "none" "!args" argsNodes], newSymbols)

typeCallArgs :: [Tree] -> [(String, [String])] -> [String] -> ([TypedTree], [(String, [String])])
typeCallArgs [] symbols prototype = ([],symbols)
typeCallArgs args symbols prototype =
  let Node _ [expr] = (head args);
      (node,newSymbols) = typeExpression expr symbols;
      argType = (head prototype);
      (nextNode,newSymbols2) = typeCallArgs (tail args) newSymbols (tail prototype)
  in (((TypedNode argType "arg" [node]) : nextNode), newSymbols2)

typePrimary :: Tree -> [(String, [String])] -> (TypedTree, [(String, [String])])
typePrimary expr symbols =
  let Node primary _ = expr;
      ret | (isDecimal primary) = (TypedNode "int" primary [], symbols)
          | (isDouble primary) = (TypedNode "double" primary [], symbols)
          | (isIdentifier primary) =
              let maybeIdType = (lookup primary symbols) in
                if (maybeIdType == Nothing)
                then errorWithoutStackTrace ("Undefined symbol: " ++ primary)
                else let (Just (idType:prototype)) = maybeIdType in
                  if (null prototype)
                  then (TypedNode idType primary [], symbols)
                  else errorWithoutStackTrace ("Function '" ++ primary ++ "' is used as variable")
          | otherwise = errorWithoutStackTrace ("Unknown primary: " ++ primary)
  in ret
