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
            } --deriving Show

data Type = Single HType | Rec HType Type deriving (Show, Eq) -- separar los tipos primitivos?? tiene sentido un String? se usaria para los tipos definidos por el usuario
data HType = UserDefined String | TInt | TString deriving (Show, Eq) 

data Expr = Call String [Expr] | Free String --deriving Show

type TypedParams = [(String, Type)]

--data PP = String | P Int deriving (Show, Eq)   ES POSIBLE DEFINIR String como un VALOR del tipo PP



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
prettyPrinterHType TInt = "Int"
prettyPrinterHType TString = "String"

pp :: Maybe DefFun -> String
pp Nothing = "Syntax error"
pp (Just fun) = prettyPrinter fun

--typeChecking :: DefFun -> StReader [(String, Type)] TypedParams Type
--typeChecking (Def name params body) = 


typeCheckingExpr :: Expr -> StReader [(String, Type)] TypedParams Type -- PRIMERO NO DEBERIA BUSCAR LA FUNC ENTRE LOS PARAM??
typeCheckingExpr (Call name subexprs) = do params <- ask
                                           funcs <- get
                                           case lookup name params <|> lookup name funcs of
                                                  Nothing -> throwExc
                                                  Just t -> return t
                                           
 
                                           
-- si esta entre los param, tiparlo
-- si no, buscar entre las func
--	si esta, tiparlo
--	si no, hay que procesar la llamada


typeCheckingExpr (Free vble) =  lookupVble vble

check :: [Expr] -> Type -> StReader [(String, Type)] TypedParams Bool
check [] _ = return False
check (exp:[]) (t@(Single htype)) = do ty <- typeCheckingExpr exp
                                       return $ ty == t
check (exp:exps) (Rec htype rtype) = do ty <- typeCheckingExpr exp
                                        rr <- check exps rtype 
                                        return $ ty == htype && rr    --- FIXME ver como reescribir los tipos, buscar la gram de haskell si hace falta    

reservedWords = ["case","class","data","default","deriving","do","else","forall"
  ,"if","import","in","infix","infixl","infixr","instance","let","module"
  ,"newtype","of","qualified","then","type","where"
  ,"foreign","ccall","as","safe","unsafe"]

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

notAReservedWord :: (MonadThrowable m) => String -> m String
notAReservedWord xs = guardT (notElem xs reservedWords) xs

validIdentifier :: (MonadThrowable m) => String -> m String
validIdentifier = (>>= notAReservedWord) . lower

allDifferent :: (Eq a) => [a] -> Bool
allDifferent []     = True
allDifferent (x:xs) = x `notElem` xs && allDifferent xs 

syntaxAnalyzer :: (MonadThrowable m) => DefFun -> m DefFun
syntaxAnalyzer fun = do fname <- validIdentifier . name $ fun
                        ps <- guardT (allDifferent $ map fst params ++ [fname]) params
                        mapM (validIdentifier . fst) ps
                        return fun
                     where params = parameters fun
                           

analyzeFuncSyntax :: (MonadThrowable m) => [DefFun] -> m ()
analyzeFuncSyntax funcs = do mapM syntaxAnalyzer funcs
                             guardT (allDifferent $ map name funcs) () -- FIXME DEBEN llamarse diferente a las func EXternas tmb???? puede introducir ambiguedad al crear los pares de aristas (dependencias) 
            

asx funcs = do externFuncs <- get
               return () --FIXME NO asumir que los nodes son Int!
                         

buildGraph funcs externFuncs = do edges <- foldM (kkp nodes $ externFuncs) (initEdges $ length names) funcs
                                  return $ makeGraph edges
                               where nodes = zip names [0..]   
                                     names = map name funcs

graphTopSort graph = do guardT (not $ cyclicGraph $ graph) ()
                        return $ topSort graph

processFunctions funcs externFuncs = do analyzeFuncSyntax funcs
                                        graph <- buildGraph funcs (map fst externFuncs)
                                        graphTopSort graph
initEdges :: Int -> [(Int, [Int])]
initEdges 0 = []
initEdges n = (n - 1, []) : initEdges (n - 1)

doIt Nothing = error "Errror"
doIt (Just v) = makeGraph v

--kkp :: (MonadThrowable m) => [(String, Int)] -> [String] -> [(Int,[Int])] -> DefFun -> m [(Int,[Int])]
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


func = DefFun "sum" [("a", Single $ TInt), ("b", Single $ TInt)] (Call "show" [Free "a", Free "b"])
func2 = DefFun "show" [("a", Single $ TInt), ("b", Single $ TInt)] (Call "print" [Free "a", Free "ccccccccccccc"])
func3 = DefFun "exc" [("a", Single $ TInt), ("b", Single $ TInt)] (Call "sum" [Free "a", Free "b"])
--typ = Rec TString (Rec TInt (Single TInt))
