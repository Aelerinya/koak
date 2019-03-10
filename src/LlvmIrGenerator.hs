module LlvmIrGenerator (generateLlvmIr) where

import Debug.Trace
import TypedTreeData
import GenerateExpression
import ConvertType

generateLlvmIr :: TypedTree -> String
generateLlvmIr program =
  let TypedNode programType _ kdefs = program;
      (codeFunctions,codeMain,lastTmp,lastHandle) = generateKdefs kdefs 1 "i32 0";
      display = (if (programType == "double")
                 then displayDouble lastHandle
                 else displayInt lastHandle)
  in constants ++ codeFunctions ++ mainDeclaration ++ codeMain ++ display ++ mainEnd


constants = "target triple = \"x86_64-pc-linux-gnu\"\n"
  ++"@.printfInt = private unnamed_addr constant [5 x i8] c\"%i\\0A\\00\\00\"\n"
  ++"@.printfDouble = private unnamed_addr constant [5 x i8] c\"%lf\\0A\\00\"\n"
  ++"declare i32 @printf(i8 *, ...)\n\n"

mainDeclaration = "define i32 @main() {\n"
mainEnd = "ret i32 0\n}\n"

displayInt id =
  "\n%_cast = getelementptr [5 x i8], [5 x i8]* @.printfInt, i64 0, i64 0\n"
  ++"call i32 (i8 *, ...) @printf(i8 * %_cast, "++ id ++")\n"

displayDouble id =
  "\n%_cast = getelementptr [5 x i8], [5 x i8]* @.printfDouble, i64 0, i64 0\n"
  ++"call i32 (i8 *, ...) @printf(i8 * %_cast, "++ id ++")\n"

generateKdefs :: [TypedTree] -> Int -> String -> (String, String, Int, String)
generateKdefs [] tmp prevHandle = ([],[],tmp,prevHandle)
generateKdefs kdefs tmp prevHandle =
  let TypedNode typeKdef kdef args = (head kdefs);
  in if (kdef == "defs")
     then let newDef = generateDef args;
              (codeFunctions,codeMain,newTmp,lastHandle) =
                generateKdefs (tail kdefs) tmp prevHandle
          in (newDef ++ codeFunctions,codeMain,newTmp,lastHandle)
     else let (newCode,newTmp,handle) = separateExpressions args tmp prevHandle;
              (codeFunctions,codeMain,newTmp2,lastHandle)
                = generateKdefs (tail kdefs) newTmp handle
          in (codeFunctions, newCode ++ codeMain,newTmp2, lastHandle)

generateDef :: [TypedTree] -> String
generateDef def =
  let [TypedNode initType initSymbol _,
       TypedNode _ _ args,
       TypedNode exprsType _ exprs] = def;
      declaration = "define " ++ (convertTypeName initType) ++ " @" ++ initSymbol;
      (prototype,initialization) = generateArguments args
      arguments = "(" ++ prototype ++ ")";
      (code,_,lastHandle) = separateExpressions exprs 1 "i32 0"
  in declaration ++ arguments ++ " {\n" ++ initialization ++ "\n"
     ++ code ++ "ret " ++ lastHandle ++ "\n}\n\n"

generateArguments :: [TypedTree] -> (String, String)
generateArguments [] = ("void","")
generateArguments args =
  let TypedNode rawType argName _ = (head args);
      argType = convertTypeName rawType;
      argHandle = argType ++ " %" ++ argName
      argAllocHandle = "%" ++ (if (argType == "double") then "d" else "i") ++ "_" ++ argName;
      argInit = "%i_" ++ argName ++ " = alloca i32\n" ++
                "%d_" ++ argName ++ " = alloca double\n"
                ++ "store " ++ argHandle ++ ", " ++ argType ++ "* " ++ argAllocHandle ++ "\n"
      (nextArg,nextInit) = (if (null (tail args))
                            then ("","")
                            else generateArguments (tail args));
  in (argHandle ++ (if (null nextArg) then "" else ", " ++ nextArg),
      argInit ++ nextInit)

separateExpressions :: [TypedTree] -> Int -> String -> (String, Int, String)
--separateExpressions exprs tmp | trace (show exprs) False = ("",0,"")
separateExpressions exprs tmp prevHandle =
  let TypedNode typeExpr expr args = (head exprs);
      res | expr == "for" = generateFor exprs tmp prevHandle
          | expr == "while" = generateWhile exprs tmp prevHandle
          | expr == "if" = generateIf exprs tmp prevHandle
          | otherwise = generateExpressions exprs tmp
  in res

generateExpressions :: [TypedTree] -> Int -> (String, Int, String)
--generateExpressions exprs tmp | trace (show exprs) False = ("",0,"")
generateExpressions [] tmp = ("",tmp,"")
generateExpressions exprs tmp =
  let (code,newTmp,handle) = generateExpression (head exprs) tmp;
      (nextCode,newTmp2,lastHandle) = generateExpressions (tail exprs) newTmp
  in (code ++ nextCode, newTmp2, if (null lastHandle) then handle else lastHandle)

generateBool :: TypedTree -> Int -> (String, Int, String)
generateBool expr tmp =
  let (codeValue,newTmp,handleValue) = generateExpression expr tmp;
      (opCode,opEnd) = (if ((head handleValue) == 'd')
                        then ("fcmp one","0.0")
                        else ("icmp ne","0"));
      id = "%" ++ (show newTmp);
      codeBool = id ++ " = " ++ opCode ++ " " ++ handleValue ++ ", " ++ opEnd ++ "\n";
      handle = "i1 " ++ id
  in (codeValue ++ codeBool,newTmp+1,handle)

generateIf :: [TypedTree] -> Int -> String -> (String, Int, String)
generateIf expr tmp prevHandle =
  let TypedNode rawType _ (TypedNode condRawType _ [conditionArg] :
                           TypedNode thenRawType _ [TypedNode _ _ thenExprs] :
                           maybeElse) = (head expr);
      ifType = convertTypeName rawType;
      conditionType = convertTypeName condRawType;
      thenType = convertTypeName thenRawType;
      isThereElse = (not (null maybeElse))
      (conditionCode,newTmp,conditionHandle) = generateBool conditionArg tmp;
      (codeConvertPrev,newTmpPrev,convertPrevHandle) = convertType ifType newTmp prevHandle;
      (thenCode,newTmp2,thenHandle) = separateExpressions thenExprs newTmpPrev prevHandle;
      (elseCode,newTmp5,elseHandle) =
        (if isThereElse
         then let TypedNode _ _ [TypedNode _ _ elseExprs] = (head maybeElse);
                  (tmpCode,newTmp3,tmpHandle) =
                    separateExpressions elseExprs newTmp2 prevHandle;
                  (convertCode,newTmp4,convertHandle) = convertType ifType newTmp3 tmpHandle
              in (tmpCode ++ convertCode,newTmp4,convertHandle)
         else ("",newTmp2,""));
      thenLabel = "Then" ++ (show tmp);
      endLabel = "End" ++ (show tmp);
      elseLabel = "Else" ++ (show tmp);
      storeCode h = "store " ++ h ++ ", " ++ ifType ++ "* %result" ++ (show tmp) ++ "\n";
      initCode = "%result" ++ (show tmp) ++ " = alloca " ++ ifType ++ "\n" ++
                 codeConvertPrev ++ (storeCode convertPrevHandle);
      resultId = "%" ++ (show newTmp5);
      endCode = resultId ++ " = load " ++ ifType ++ ", "
                ++ ifType ++ "* %result" ++ (show tmp) ++ "\n\n";
      ifCode = "\n" ++ conditionCode ++
               initCode ++
               "br " ++ conditionHandle ++ ", label %" ++ thenLabel ++ ", label %"
               ++ (if (isThereElse) then elseLabel else endLabel) ++ "\n" ++
               thenLabel ++ ":\n" ++
               thenCode ++
               (storeCode thenHandle) ++
               "br label %" ++ endLabel ++ "\n" ++
               (if isThereElse
                then elseLabel ++ ":\n" ++
                     elseCode ++
                     (storeCode elseHandle)++
                     "br label %" ++ endLabel ++ "\n"
                else "") ++
               endLabel ++ ":\n" ++ endCode;
      handle = ifType ++ " " ++ resultId
  in (ifCode,newTmp5,handle)

generateWhile :: [TypedTree] -> Int -> String -> (String, Int, String)
generateWhile expr tmp prevHandle =
  let TypedNode rawType _ [TypedNode _ _ [conditionExpr],
                           TypedNode _ _ doExprs] = (head expr);
      whileType = convertTypeName rawType;
      (conditionCode,newTmp,conditionHandle) = generateBool conditionExpr tmp;
      (doCode,newTmp2,doHandle) = separateExpressions doExprs newTmp prevHandle;
      startLabel = "While" ++ (show tmp);
      doLabel = "Do" ++ (show tmp);
      endLabel = "End" ++ (show tmp);
      whileCode = "\n" ++ "br label %" ++ startLabel ++ "\n" ++
                  startLabel ++ ":\n" ++
                  conditionCode ++
                  "br " ++ conditionHandle ++ ", label %" ++ doLabel
                  ++ ", label %" ++ endLabel ++ "\n" ++
                  doLabel ++ ":\n" ++
                  doCode ++
                  "br label %" ++ startLabel ++ "\n" ++
                  endLabel ++ ":\n\n";
      handle = doHandle
  in (whileCode,newTmp2,handle)

generateFor :: [TypedTree] -> Int -> String -> (String, Int, String)
generateFor expr tmp prevHandle =
  let TypedNode rawType _ [TypedNode _ _ [initArg],
                           TypedNode _ _ [conditionArg],
                           TypedNode _ _ [incrementArg],
                           TypedNode _ _ inExprs] = (head expr);
      forType = convertTypeName rawType;
      (initCode,newTmp,initHandle) = generateExpression initArg tmp;
      (conditionCode,newTmp2,conditionHandle) = generateBool conditionArg newTmp;
      (inCode,newTmp3,inHandle) = separateExpressions inExprs newTmp2 prevHandle;
      (incrementCode,newTmp4,incrementHandle) = generateExpression incrementArg newTmp3;
      conditionLabel = "For" ++ (show tmp);
      inLabel = "In" ++ (show tmp);
      endLabel = "End" ++ (show tmp);
      forCode = "\n" ++ initCode ++
                "br label %" ++ conditionLabel ++ "\n" ++
                conditionLabel ++ ":\n" ++
                conditionCode ++
                "br " ++ conditionHandle ++ ", label %" ++ inLabel
                ++ ", label %" ++ endLabel ++ "\n" ++
                inLabel ++ ":\n" ++
                inCode ++
                incrementCode ++
                "br label %" ++ conditionLabel ++ "\n" ++
                endLabel ++ ":\n\n";
      handle = inHandle
  in (forCode,newTmp4,handle)
