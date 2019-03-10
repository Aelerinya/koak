module GenerateExpression (generateExpression) where

import Debug.Trace
import TypedTreeData
import Identifier
import Decimal
import Double
import ConvertType

unops = ["-","!"]
generateExpression :: TypedTree -> Int -> (String, Int, String)
--generateExpression expr tmp | trace (show expr) False = ("",0,"")
generateExpression expr tmp =
  let TypedNode opType op args = expr;
      res | (op == "!call") = generateCall expr tmp
          | ((length args) == 2) = generateBinops expr tmp
          | (elem op unops) = generateUnop expr tmp
          | otherwise = generatePrimary expr tmp
  in res

generateCall :: TypedTree -> Int -> (String, Int, String)
generateCall expr tmp =
  let TypedNode _ _ [TypedNode rawType callId _, TypedNode _ _ args] = expr;
      (argsCode,newTmp,argsHandles,prototype) = generateArgs args tmp;
      callType = convertTypeName rawType;
      result = "%" ++ (show newTmp);
      assignation = (if (callType == "void") then "" else result ++ " = ");
      code = assignation ++ "call " ++ callType ++ " (" ++ prototype ++ ") @" ++ callId
             ++ "(" ++ argsHandles ++ ")\n";
      handle = (if (callType == "void") then "" else callType ++ " " ++ result)
      newTmp2 = (if (callType == "void") then newTmp else newTmp + 1)
  in (argsCode ++ code,newTmp2,handle)

generateArgs :: [TypedTree] -> Int -> (String, Int, String, String)
generateArgs [] tmp = ("",tmp,"","")
generateArgs args tmp =
  let TypedNode rawType _ [expr] = (head args);
      argType = convertTypeName rawType;
      (codeArg,newTmp,handleArg) = generateExpression expr tmp;
      (codeConversion,newTmp2,handleConversion) = convertType argType newTmp handleArg
  in if (null (tail args))
     then (codeArg ++ codeConversion, newTmp2, handleConversion, argType)
     else let (codeNext,newTmp3,handleNext,prototypeNext) = generateArgs (tail args) newTmp2
          in (codeArg ++ codeConversion ++ codeNext,
              newTmp3,
              handleConversion ++ ", " ++ handleNext,
              argType ++ ", " ++ prototypeNext)

generateUnop :: TypedTree -> Int -> (String, Int, String)
generateUnop expr tmp =
  let TypedNode _ unop _ = expr
  in if (unop == "!")
     then generateNot expr tmp
     else generateNegative expr tmp

generateNot :: TypedTree -> Int -> (String, Int, String)
generateNot expr tmp =
  let TypedNode rawType _ [arg] = expr;
      (codeArg,newTmp,handleArg) = generateExpression arg tmp;
      unopType = convertTypeName rawType;
      id = "%" ++ (show newTmp);
      code = id ++ " = " ++ (if (unopType == "double") then "fcmp oeq" else "icmp eq")
             ++ " " ++ handleArg ++ ", "
             ++ (if (unopType == "double") then "0.0" else "0") ++ "\n";
      conversionId = "%" ++ (show (newTmp + 1));
      conversion = (if (unopType == "double")
                    then conversionId ++ " = uitofp i1 " ++ id ++ " to double\n"
                    else conversionId ++ " = zext i1 " ++ id ++ " to i32\n");
      handle = unopType ++ " " ++ conversionId
  in (codeArg ++ code ++ conversion, newTmp + 2, handle)

generateNegative :: TypedTree -> Int -> (String, Int, String)
generateNegative expr tmp =
  let TypedNode rawType _ [arg] = expr;
      (codeArg,newTmp,handleArg) = generateExpression arg tmp;
      unopType = convertTypeName rawType;
      id = "%" ++ (show newTmp);
      code = id ++ " = " ++ (if (unopType == "double") then "fmul" else "mul")
             ++ " " ++ handleArg ++ ", "
             ++ (if (unopType == "double") then "-1.0" else "-1") ++ "\n";
      handle = unopType ++ " " ++ id
  in (codeArg ++ code, newTmp + 1, handle)

generatePrimary :: TypedTree -> Int -> (String, Int, String)
generatePrimary primary tmp =
  let TypedNode primaryType primaryName _ = primary;
      handle = (convertTypeName primaryType)
             ++ (" " ++ if ((head primaryName) == '.')
                        then "0" ++ primaryName
                        else primaryName)
  in if (isIdentifier primaryName)
  then generateVariableLoad primary tmp
  else ("", tmp, handle)

generateVariableLoad :: TypedTree -> Int -> (String, Int, String)
generateVariableLoad primary tmp =
  let TypedNode rawIdType id _ = primary;
      idType = convertTypeName rawIdType
      code = "%" ++ (show tmp) ++ " = load " ++ idType ++ ", " ++ idType ++ "* %"
             ++ (if (idType == "double") then "d" else "i") ++ "_" ++ id ++ "\n";
      handle = idType ++ " %" ++ (show tmp)
  in (code,tmp + 1,handle)

combineHandles :: String -> String -> String
combineHandles handle1 handle2 =
  let (type1,id1) = break (== ' ') handle1;
      (type2,id2) = break (== ' ') handle2
  in (handle1 ++ "," ++ id2)

generateBinops :: TypedTree -> Int -> (String, Int, String)
generateBinops expr tmp =
  let TypedNode _ binop args = expr
  in if (elem binop arithmetic)
     then generateArithmetic expr tmp
     else if (elem binop comparison)
          then generateComparison expr tmp
          else generateAssignement expr tmp

arithmetic = ["+","*","-","/"]
arithmeticDouble = [("+","fadd"),("-","fsub"),("*","fmul"),("/","fdiv")]
arithmeticInt = [("+","add"),("-","sub"),("*","mul"),("/","sdiv")]

generateArithmetic :: TypedTree -> Int -> (String, Int, String)
generateArithmetic expr tmp =
  let TypedNode opType op [arg1,arg2] = expr;
      (code1,newTmp,handle1) = generateExpression arg1 tmp;
      (code2,newTmp2,handle2) = generateExpression arg2 newTmp;
      (codeConvert1,newTmp3,handleConvert1) = convertType opType newTmp2 handle1;
      (codeConvert2,newTmp4,handleConvert2) = convertType opType newTmp3 handle2;
      handle1and2 = combineHandles handleConvert1 handleConvert2;
      (Just opCode) = lookup op (if (opType == "int")
                                 then arithmeticInt
                                 else arithmeticDouble);
      code = "%" ++ (show newTmp4) ++ " = "
             ++ opCode ++ " " ++ handle1and2 ++ "\n";
      handle = (convertTypeName opType) ++ " %" ++ (show newTmp4)
  in (code1 ++ code2 ++ codeConvert1 ++ codeConvert2 ++ code, newTmp4 + 1, handle)

comparison = ["<",">","==","!="]
comparisonInt = [("<","slt"),(">","sgt"),("==","eq"),("!=","ne")]
comparisonDouble = [("<","olt"),(">","ogt"),("==","oeq"),("!=","one")]

generateComparison :: TypedTree -> Int -> (String, Int, String)
generateComparison expr tmp =
  let TypedNode opType op [arg1,arg2] = expr;
      (code1,newTmp,handle1) = generateExpression arg1 tmp;
      (code2,newTmp2,handle2) = generateExpression arg2 newTmp;
      wantedType = (if ((head handle1) == 'd' || (head handle2) == 'd')
                    then "double" else "i32")
      (codeConvert1,newTmp3,handleConvert1) = convertType wantedType newTmp2 handle1;
      (codeConvert2,newTmp4,handleConvert2) = convertType wantedType newTmp3 handle2;
      handle1and2 = combineHandles handleConvert1 handleConvert2;
      (Just opCode) = lookup op (if (wantedType == "int")
                                 then comparisonInt
                                 else comparisonDouble);
      boolId = "%" ++ (show newTmp4);
      resultId = "%" ++ (show (newTmp4 + 1));
      comparisonCode = boolId ++ " = "
                       ++ (if (wantedType == "i32") then "icmp " else "fcmp ")
                       ++ opCode ++ " " ++ handle1and2 ++ "\n";
      conversionCode = resultId ++ " = zext i1 " ++ boolId ++ " to i32\n";
      handle = "i32 " ++ resultId
  in (code1 ++ code2 ++ codeConvert1 ++ codeConvert2 ++ comparisonCode ++ conversionCode,
      newTmp4 + 2, handle)

generateAssignement :: TypedTree -> Int -> (String, Int, String)
generateAssignement expr tmp =
  let TypedNode rawValueType _ [TypedNode _ id [TypedNode _ status _],value] = expr;
      (codeValue,newTmp,valueHandle) = generateExpression value tmp;
      valueType = convertTypeName rawValueType;
      pointer = "%" ++ (if (valueType == "double") then "d" else "i")
                ++ "_" ++ id;
      allocation = if (status /= "first") then ""
                   else "%i_" ++ id ++ " = alloca i32\n"
                        ++ "%d_" ++ id ++ " = alloca double\n";
      storage = "store " ++ valueHandle ++ ", " ++ valueType ++ "* " ++ pointer ++ "\n"
      in (codeValue ++ allocation ++ storage, newTmp, valueHandle)
