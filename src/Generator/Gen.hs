module Generator.Gen where

import Generator.StReader
import Data.Char
import Control.Monad
import Control.Applicative
import Generator.DGraph
--data DataType = DataType 
--              {


data DefFun = DefFun
            { name       :: String
            , parameters :: [(String, Type)] -- un param puede ser una funcion, deberia poder usarse esa funcion localmente
            , body       :: Expr
            } --deriving Show

data Type = Single HType | Rec Type Type deriving (Show, Eq)
data HType = UserDefined String | TInt | TString deriving (Show, Eq) -- LISTAS??

data Expr = Call String [Expr] | Free String --deriving Show

type TypedParams = [(String, Type)]

--data PP = String | P Int deriving (Show, Eq)   ES POSIBLE DEFINIR String como un VALOR del tipo PP



--Call "sum" [Free "a", Free "b"]
--"sum" => T "Int" (T "Int" (T "Int"))

-- Iterar sobre el tipo y devolver lo que falta

--myFun :: Int -> Int
--myFun a = sum 3 a

prettyPrinter :: DefFun -> String
prettyPrinter fun = name fun ++ prettyPrinterParam (parameters fun) ++ " = " ++ prettyPrinterExpr (body fun)  

prettyPrinterExpr :: Expr -> String
prettyPrinterExpr (Call name xs) = name ++ (foldl (\res expr -> res ++ " " ++ (prettyPrinterExpr expr)) "" xs)  
prettyPrinterExpr (Free vble) = vble  

prettyPrinterParam :: [(String, Type)] -> String
prettyPrinterParam xs = foldl (\res (str, ty) -> res ++ " " ++ str) "" xs

prettyPrinterType :: Type -> String
prettyPrinterType (Single htype) = prettyPrinterHType htype
prettyPrinterType (Rec ttype ttype') = "(" ++ prettyPrinterType ttype ++ " -> " ++ prettyPrinterType ttype' ++ ")"

prettyPrinterHType :: HType -> String
prettyPrinterHType (UserDefined udtype) = udtype
prettyPrinterHType TInt = "Int"
prettyPrinterHType TString = "String"

pp :: Maybe DefFun -> String
pp Nothing = "Syntax error"
pp (Just fun) = prettyPrinter fun

--typeChecking :: DefFun -> StReader [(String, Type)] TypedParams Type
typeChecking (DefFun name params body) = do bodyType <- local (const params) $ typeCheckingExpr body
                                            funcs <- get
                                            funcType <- return $ foldr (\ (_,t) ty -> Rec t ty) bodyType params
                                            put $ (name, funcType):funcs  -- FIXME el client de este metodo deberia hacer este put
                                            return $ funcType

--typeCheckingExpr :: Expr -> StReader [(String, Type)] TypedParams Type
typeCheckingExpr (Call name subexprs) = do params <- ask
                                           funcs <- get
                                           case lookup name params <|> lookup name funcs of -- FIXME arreglar este case
                                                           Nothing -> throwExc
                                                           Just t -> check subexprs t 
typeCheckingExpr (Free vble) =  lookupVble vble


--check :: [Expr] -> Type -> StReader [(String, Type)] TypedParams Type
check [] t = return t
--check (exp:exps) (t@(Single htype)) = 
check (exp:exps) (Rec ttype ttype') = do ty <- typeCheckingExpr exp
                                         if ty == ttype then check exps ttype' else throwExc    
check _ _ = throwExc
reservedWords = ["case","class","data","default","deriving","do","else","forall"
  ,"if","import","in","infix","infixl","infixr","instance","let","module"
  ,"newtype","of","qualified","then","type","where"
  ,"foreign","ccall","as","safe","unsafe"]

--lookupVble :: Eq key => key -> StReader s [(key, value)] value
lookupVble vble = do params <- ask
                     case lookup vble params of                  --- FIXME
                         Nothing -> throwExc
                         Just t -> return t


class (Monad m, Alternative m) => MonadThrowable m where
      throw :: String -> m a
      guardT :: Bool -> a -> m a -- Agregar String para informar el error?

instance MonadThrowable Maybe where
      throw _ = Nothing
      guardT = \x -> (guard x >>) . Just


instance MonadThrowable (StReader s e) where
      throw _ = throwExc
      guardT = \x -> (guard x >>) . return

instance Alternative (StReader s e) where
      empty = StReader (\_ _ -> Nothing)
      StReader f <|> (StReader g) = StReader (\ s e -> f s e <|> g s e)  


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
               processFunctions funcs externFuncs --of ------ FIXME ver este case
--                     Nothing -> throwExc
--                     Just vs -> mapM (\v -> typeChecking (funcs !! v)) vs
               get --FIXME NO asumir que los nodes son Int!
                       


--Gen.hs:145:1: error:
--    • Non type-variable argument
--        in the constraint: MonadThrowable (StReader [(String, b)] e)
--      (Use FlexibleContexts to permit this)
--    • When checking the inferred type
--        asx :: forall e b.
--               MonadThrowable (StReader [(String, b)] e) =>
--               [DefFun] -> StReader [(String, b)] e ()  

run = f $ (runStReader $ asx [func, func2, func3, func4]) [("print",typ)] []
      where f Nothing = error "Nothing"
            f (Just (s,_)) = map (\(n,t) -> n ++ " :: " ++ prettyPrinterType t) s

buildGraph funcs externFuncs = do edges <- foldM (kkp nodes $ externFuncs) (initEdges $ length names) funcs
                                  return $ makeGraph edges
                               where nodes = zip names [0..]   
                                     names = map name funcs

graphTopSort graph = do guardT (not $ cyclicGraph $ graph) ()
                        return $ topSort graph

--processFunctions :: (MonadThrowable m) => [DefFun] -> [(String,Type)] -> m [Type]
processFunctions funcs externFuncs = do analyzeFuncSyntax funcs
                                        graph <- buildGraph funcs (map fst externFuncs)
                                        vs <- graphTopSort graph
                                        mapM (\v -> typeChecking (funcs !! v)) vs
                                       
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

lookupT :: (MonadThrowable m, Eq a) => a -> [(a,b)] -> m b
lookupT _ [] = throw $ "Unable to find this key"
lookupT x ((y,z):ys) | x == y = return z
                     | otherwise = lookupT x ys


addT :: Eq a => [(a, [b])] -> a -> b -> [(a, [b])]
addT [] a b = [(a, [b])]
addT ((x,bs):ys) a b | x == a = [(x, b:bs)] ++ ys
                     | otherwise = (x,bs) : addT ys a b

func = DefFun "sum" [("a", Single $ TInt), ("b", Single $ TInt), ("c", Single $ TString)] (Call "show" [Free "a", Free "b"])
func2 = DefFun "show" [("a", Single $ TInt), ("b", Single $ TInt)] (Call "print" [Free "a", Free "b"])
func3 = DefFun "exc" [("a", Single $ TInt), ("b", Single $ TString)] (Call "sum" [Free "a", Free "a"])
func4 = DefFun "apply" [("sum", Rec (Single TString) (Single TInt)), ("b", Single $ TString)] (Call "sum" [Free "b"])
typ = Rec (Single TInt) (Rec (Single TInt) (Single TString))
