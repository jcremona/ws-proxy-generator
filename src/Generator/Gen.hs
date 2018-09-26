import StReader
import Data.Char
import Control.Monad
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

type TypedParams = [(String, Type)]

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

pp :: Maybe DefFun -> String
pp Nothing = "Syntax error"
pp (Just fun) = prettyPrinter fun

--typeChecking :: DefFun -> StReader () TypedParams Type
--typeChecking (Def name params body) = 

-- VER PALABRAS RESERVADAS

--typeCheckingExpr :: Expr -> StReader [(String, Type)] TypedParams Type -- PRIMERO NO DEBERIA BUSCAR LA FUNC ENTRE LOS PARAM??
--typeCheckingExpr (Call name subexprs) = do params <- get
--                                           case lookup name params of
--                                                  Nothing -> throw
--                                                  Just t -> t
                                           
 
                                           
-- si esta entre los param, tiparlo
-- si no, buscar entre las func
--	si esta, tiparlo
--	si no, hay que procesar la llamada


--typeCheckingExpr (Free vble) =  lookupVble vble

--check :: [Expr] -> Type -> Bool
--check exps ty = foldl (\ exp -> typeCheckingExpr exp 


lookupVble :: Eq key => key -> StReader s [(key, value)] value
lookupVble vble = do params <- ask
                     case lookup vble params of
                         Nothing -> throwExc
                         Just t -> return t


class Monad m => MonadThrowable m where
      throw :: String -> m a
      guardT :: Bool -> a -> m a -- Agregar String para informar el error?

instance MonadThrowable Maybe where
      throw _ = Nothing
      guardT = \x -> (guard x >>) . Just


lower :: (MonadThrowable m) => String -> m String
lower [] = throw "Invalid identifier (empty String)"
lower (x:xs) = guardT (isLower x) (x:xs)

allDifferent :: (Eq a) => [a] -> Bool
allDifferent []     = True
allDifferent (x:xs) = x `notElem` xs && allDifferent xs 

syntaxAnalyzer :: (MonadThrowable m) => DefFun -> m DefFun
syntaxAnalyzer fun = do fname <- lower . name $ fun
                        ps <- guardT (allDifferent $ map fst params ++ [fname]) params
                        mapM (lower . fst) ps
                        return fun
                     where params = parameters fun

asx :: (MonadThrowable m) => [DefFun] -> m [DefFun]
asx funcs = do mapM syntaxAnalyzer funcs
               guardT (allDifferent $ map name funcs) funcs
-- params podria ser Reader, y lista de func ya tipadas podria ser state

func = DefFun "sum" [("a", Single $ Prim TInt), ("b", Single $ Prim TInt)] (Call "show" [Free "a", Free "b"])
--func2 = DefFun "sum" [("a", Single $ Prim TInt), ("b", Single $ Prim TInt)] (Call "show" [Free "a", Free "b"])
--typ = Rec (Prim TString) (Rec (Prim TInt) (Single $ Prim TInt))
