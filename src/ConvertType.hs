module ConvertType where

convertTypeName :: String -> String
convertTypeName t =
  if (t == "int")
  then "i32"
  else t

convertType :: String -> Int -> String -> (String, Int, String)
convertType wantedType tmp handle =
  let (currentType,handleId) = break (== ' ') handle;
      convertedWantedType = convertTypeName wantedType
  in if (convertedWantedType == currentType)
     then ("",tmp,handle)
     else let code = "%" ++ (show tmp) ++ " = " ++
                     (if (wantedType == "double") then "sitofp " else "fptosi ")
                     ++ handle ++ " to " ++ convertedWantedType ++ "\n";
              newHandle = convertedWantedType ++ " %" ++ (show tmp)
          in (code,tmp + 1,newHandle)
