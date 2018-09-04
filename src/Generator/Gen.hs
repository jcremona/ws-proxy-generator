--data DataType = DataType 
--              {

data DefFun = DefFun
            { name       :: String
            , parameters :: [(String, Type)] -- un param puede ser una funcion, deberia poder usarse esa funcion localmente
            , body       :: Expr
            }

data Type = Single HType | Rec HType Type -- separar los tipos primitivos?? tiene sentido un String? se usaria para los tipos definidos por el usuario

data HType = UserDefined String | Prim Primitive 

data Primitive = TInt | TString

data Expr = Call String [Expr] | Free String 


--Call "sum" [Free "a", Free "b"]
--"sum" => T "Int" (T "Int" (T "Int"))

-- Iterar sobre el tipo y devolver lo que falta

--myFun :: Int -> Int
--myFun a = sum 3 a

-- permite alto orden al llamar a una func? Hay Diferentes tipos de llamada a una misma func

-- al tipar una llamada a funcion, busco en una lista de func ya tipadas
--	si ya estaba en la lista, chequeo que el tipo coincida

prettyPrinter :: DefFun -> String
prettyPrinter fun = name fun ++ prettyPrinterParam (parameters fun) ++ " = " ++ prettyPrinterExpr (body fun)  

prettyPrinterExpr :: Expr -> String
prettyPrinterExpr (Call name xs) = name ++ (foldl (\res expr -> res ++ " " ++ (prettyPrinterExpr expr)) "" xs)  
prettyPrinterExpr (Free vble) = vble  

prettyPrinterParam :: [(String, Type)] -> String
prettyPrinterParam xs = foldl (\res (str, ty) -> res ++ " " ++ str) "" xs

prettyPrinterType :: Type -> String
prettyPrinterType (Single htype) = prettyPrinterHType htype
prettyPrinterType (Rec htype ttype) = prettyPrinterHType htype ++ " -> " ++ prettyPrinterType ttype

prettyPrinterHType :: HType -> String
prettyPrinterHType (UserDefined udtype) = udtype
prettyPrinterHType (Prim TInt) = "Int"
prettyPrinterHType (Prim TString) = "String"

--func = DefFun "sum" [("a", Single $ Prim TInt), ("b", Single $ Prim TInt)] (Call "show" [Free "a", Free "b"])
--typ = Rec (Prim TString) (Rec (Prim TInt) (Single $ Prim TInt))
