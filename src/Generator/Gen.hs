import StReader
import Data.Char
import Control.Monad
import Control.Applicative
import DGraph
--data DataType = DataType 
--              {


data DefFun = DefFun
            { name       :: String
            , parameters :: [(String, Type)] -- un param puede ser una funcion, deberia poder usarse esa funcion localmente
            , body       :: Expr
            } deriving Show

data Type = Single HType | Rec HType Type deriving Show -- separar los tipos primitivos?? tiene sentido un String? se usaria para los tipos definidos por el usuario
data HType = UserDefined String | Prim Primitive deriving Show 
data Primitive = TInt | TString deriving Show

data Expr = Call String [Expr] | Free String deriving Show

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


class (Monad m, Alternative m) => MonadThrowable m where
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

analyzeFuncSyntax :: (MonadThrowable m) => [DefFun] -> m [String]
analyzeFuncSyntax funcs = do mapM syntaxAnalyzer funcs
                             guardT (allDifferent $ names) names -- DEBEN llamarse diferente a las func EXternas tmb????
            where names = map name funcs

-- ver como meter las funciones externas

asx funcs = do names <- analyzeFuncSyntax funcs
               foldM (kkp (getNodes names) ["print"]) (initEdges (length names)) funcs --FIXME NO asumir que los nodes son Int!
            where getNodes names = zip names [0..]             

initEdges :: Int -> [(Int, [Int])]
initEdges 0 = []
initEdges n = (n - 1, []) : initEdges (n - 1)

doIt Nothing = error "Errror"
doIt (Just v) = makeGraph v
mb :: Maybe [(Int, [Int])]
mb = asx [func,func2, func3]

runn = doIt $ mb

kkp :: (MonadThrowable m) => [(String, Int)] -> [String] -> [(Int,[Int])] -> DefFun -> m [(Int,[Int])]
kkp xs externFuncs ys fun = do node <- lookupT (name fun) xs
                               kexpr node . body $ fun
                where kexpr n (Call name' exprs) = guardT (elem name' $ paramFuncs ++ externFuncs) ys <|> 
                                                   (do dd <- lookupT name' xs
                                                       return $ addT ys dd n)  
                      kexpr n (Free vble) = return ys
                      paramFuncs = map fst $ parameters fun                 

--f1 a = f2 2a
--f3 a = f1 a


--kexpr :: Expr -> Int
-- params podria ser Reader, y lista de func ya tipadas podria ser state

lookupT :: (MonadThrowable m, Eq a) => a -> [(a,b)] -> m b
lookupT _ [] = throw $ "Unable to find this key"
lookupT x ((y,z):ys) | x == y = return z
                     | otherwise = lookupT x ys


addT :: Eq a => [(a, [b])] -> a -> b -> [(a, [b])]
addT [] a b = [(a, [b])]
addT ((x,bs):ys) a b | x == a = [(x, b:bs)] ++ ys
                     | otherwise = (x,bs) : addT ys a b


func = DefFun "sum" [("a", Single $ Prim TInt), ("b", Single $ Prim TInt)] (Call "show" [Free "a", Free "b"])
func2 = DefFun "show" [("a", Single $ Prim TInt), ("b", Single $ Prim TInt)] (Call "print" [Free "a", Free "b"])
func3 = DefFun "exc" [("a", Single $ Prim TInt), ("b", Single $ Prim TInt)] (Call "sum" [Free "a", Free "b"])
--typ = Rec (Prim TString) (Rec (Prim TInt) (Single $ Prim TInt))
