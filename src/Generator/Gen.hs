module Generator.Gen where

import Generator.StReader
import Data.Char
import Data.List
import Control.Monad
import Control.Applicative
import Generator.DGraph

data DataType = DataType 
              { typeName :: String,
                constructors :: [(String, Type)]
              } deriving Show

data DefFun = DefFun
            { funname       :: String
            , funparameters :: [(String, Type)] -- un param puede ser una funcion, deberia poder usarse esa funcion localmente
            , body       :: Expr
            } deriving Show

data Module = Module
            { moduleName :: String,
              functions :: [DefFun]
            , dataTypes :: [DataType]
            } deriving Show

data Type = Single HType | Rec Type Type | TList LType | TTuple (Type, Type) deriving (Show, Eq)
data LType = Empty | T Type deriving (Show, Eq)
data HType = UserDefined String | TInt | TString | TDouble | TLong | TChar | TFloat | TVoid | TBool deriving (Show, Eq) -- LISTAS??

data Expr = Call String [Expr] | Free String | StringValue String | ListValue [Expr] | TupleValue (Expr,Expr) deriving Show

type TypedParams = [(String, Type)]

--data PP = String | P Int deriving (Show, Eq)   ES POSIBLE DEFINIR String como un VALOR del tipo PP



--Call "sum" [Free "a", Free "b"]
--"sum" => T "Int" (T "Int" (T "Int"))

-- Iterar sobre el tipo y devolver lo que falta

--myFun :: Int -> Int
--myFun a = sum 3 a

write m = writeFile ("/home/jcremona/" ++ moduleName m ++ ".hs") $ prettyPrinterModule m


prettyPrinterModule :: Module -> String
prettyPrinterModule (Module name funcs dts) = "module " ++ name ++ " where\n\n" ++ (dts >>= (++ "\n\n") . prettyPrinterDataType) ++ (funcs >>= (++ "\n\n") . prettyPrinterFun)

prettyPrinterFun :: DefFun -> String
prettyPrinterFun fun = funname fun ++ prettyPrinterParam (funparameters fun) ++ " = " ++ prettyPrinterExpr (body fun)  

prettyPrinterDataType :: DataType -> String
prettyPrinterDataType (DataType name constructors) = "data " ++ name ++ " = " ++ name ++ "{\n" ++ (intercalate "," $ map prettyPrinterConstructor constructors) ++ "} deriving (Show)\n"
                                                where prettyPrinterConstructor (n, t) = n ++ "::" ++ prettyPrinterType t ++ "\n" 

prettyPrinterExpr :: Expr -> String
prettyPrinterExpr (Call name xs) = name ++ (foldl (\res expr -> res ++ " " ++ (prettyPrinterExpr expr)) "" xs)  
prettyPrinterExpr (Free vble) = vble  
prettyPrinterExpr (StringValue str) = "\"" ++ str ++ "\""
prettyPrinterExpr (ListValue ls) = "[" ++ (intercalate "," $ map prettyPrinterExpr ls) ++ "]"--"[" ++ foldl (\res expr -> res ++ ", " ++ (prettyPrinterExpr expr)) "" ls ++ "]"
prettyPrinterExpr (TupleValue (e1,e2)) = "(" ++ prettyPrinterExpr e1 ++ "," ++ prettyPrinterExpr e2 ++ ")"

prettyPrinterParam :: [(String, Type)] -> String
prettyPrinterParam xs = foldl (\res (str, ty) -> res ++ " " ++ str) "" xs

prettyPrinterType :: Type -> String
prettyPrinterType (Single htype) = prettyPrinterHType htype
prettyPrinterType (Rec ttype ttype') = "(" ++ prettyPrinterType ttype ++ " -> " ++ prettyPrinterType ttype' ++ ")"
prettyPrinterType (TList Empty) = "[" ++ "a" ++ "]" -- FIXME arreglar esto, si una func toma dos listas como parametros, las listas pueden ser del mismo tipo a o no
prettyPrinterType (TList (T ttype)) = "[" ++ prettyPrinterType ttype ++ "]"
prettyPrinterType (TTuple (ttype, ttype')) = "(" ++ prettyPrinterType ttype ++ ", " ++ prettyPrinterType ttype' ++ ")"

prettyPrinterHType :: HType -> String
prettyPrinterHType (UserDefined udtype) = udtype
prettyPrinterHType TInt = "Int"
prettyPrinterHType TString = "String"
prettyPrinterHType TDouble = "Double"
prettyPrinterHType TLong = "Integer"
prettyPrinterHType TChar = "Char"
prettyPrinterHType TFloat = "Float"
prettyPrinterHType TVoid = "()"
prettyPrinterHType TBool = "Bool"


--pp :: Maybe DefFun -> String
--pp Nothing = "Syntax error"
--pp (Just fun) = prettyPrinter fun

--typeChecking :: DefFun -> StReader [(String, Type)] TypedParams Type
typeChecking (DefFun name params body) = do bodyType <- local (const params) $ typeCheckingExpr body
                                            funcs <- get
                                            funcType <- return $ foldr (\ (_,t) ty -> Rec t ty) bodyType params
                                            put $ (name, funcType):funcs  -- FIXME el client de este metodo deberia hacer este put
                                            return $ funcType

--typeCheckingExpr :: Expr -> StReader [(String, Type)] TypedParams Type
typeCheckingExpr (Call name subexprs) = do params <- ask
                                           funcs <- get
                                           fn $ lookup name params <|> lookup name funcs 
                                          where fn Nothing = throwExc      
                                                fn (Just t) = check subexprs t  --FIXME es necesario chequear que t es de la forma Rec () () ? 
typeCheckingExpr (Free vble) =  lookupVble vble
typeCheckingExpr (StringValue _) = return $ Single TString
--typeCheckingExpr (ListValue []) = return $ TList Empty
typeCheckingExpr (ListValue exprs) = foldM (\ res  expr -> do t1 <- typeCheckingExpr expr
                                                              if equalType (TList $ T t1) res then return $ TList $ T t1 else throwExc) (TList Empty) exprs  
typeCheckingExpr (TupleValue (e1, e2)) = do t1 <- typeCheckingExpr e1
                                            t2 <- typeCheckingExpr e2
                                            return $ TTuple (t1, t2)

--check :: [Expr] -> Type -> StReader [(String, Type)] TypedParams Type
check [] t = return t
--check (exp:exps) (t@(Single htype)) = 
check (exp:exps) (Rec ttype ttype') = do ty <- typeCheckingExpr exp
                                         if equalType ty ttype then check exps ttype' else throwExc    
check _ _ = throwExc

equalType (TList Empty) (TList (T _)) = False -- FIXME arreglar esto, o al menos comentar bien que el orden en el que se pasan los args importa
equalType (TList _) (TList Empty) = True
equalType (TList (T t1)) (TList (T t2)) = equalType t1 t2
equalType t1 t2 = t1 == t2

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

upper :: (MonadThrowable m) => String -> m String
upper [] = throw "Invalid identifier (empty String)"
upper (x:xs) = guardT (isUpper x) (x:xs)

notAReservedWord :: (MonadThrowable m) => String -> m String
notAReservedWord xs = guardT (notElem xs reservedWords) xs

validIdentifier :: (MonadThrowable m) => String -> m String
validIdentifier = (>>= notAReservedWord) . lower

allDifferent :: (Eq a) => [a] -> Bool
allDifferent []     = True
allDifferent (x:xs) = x `notElem` xs && allDifferent xs 

syntaxAnalyzer :: (MonadThrowable m) => DefFun -> m DefFun
syntaxAnalyzer fun = do fname <- validIdentifier . funname $ fun
                        ps <- guardT (allDifferent $ map fst params ++ [fname]) params
                        mapM (validIdentifier . fst) ps
                        return fun
                     where params = funparameters fun
                           

analyzeFuncSyntax :: (MonadThrowable m) => [DefFun] -> m ()
analyzeFuncSyntax funcs = do mapM syntaxAnalyzer funcs
                             guardT (allDifferent $ map funname funcs) () -- FIXME DEBEN llamarse diferente a las func EXternas tmb???? puede introducir ambiguedad al crear los pares de aristas (dependencias) 

dataTypeSyntaxAnalyzer :: (MonadThrowable m) => DataType -> m DataType
dataTypeSyntaxAnalyzer dt = do mapM (lower . fst) $ constructors dt
                               upper $ typeName dt
                               return dt
            
analyzeDataTypesSyntax :: (MonadThrowable m) => [DataType] -> m ()
analyzeDataTypesSyntax dts = do mapM dataTypeSyntaxAnalyzer dts
                                guardT (allDifferent $ map typeName dts) ()


asx funcs dts = do externFuncs <- get
                   dtFuncs <- processDataTypes dts
                   put $ externFuncs ++ dtFuncs
                   processFunctions funcs (externFuncs ++ dtFuncs)--of ------ FIXME ver este case
--                     Nothing -> throwExc
--                     Just vs -> mapM (\v -> typeChecking (funcs !! v)) vs
                   get --FIXME NO asumir que los nodes son Int!
                       

processModule moduleName funcs dts = do fs <- asx funcs dts
                                        return $ Module (upperFirstChar moduleName) funcs dts


runGenerationModule (Module moduleName funcs dts) = runStReader (processModule moduleName funcs dts) [("invokeWS",callWSType)] [] >>= Just . fst
--Gen.hs:145:1: error:
--    • Non type-variable argument
--        in the constraint: MonadThrowable (StReader [(String, b)] e)
--      (Use FlexibleContexts to permit this)
--    • When checking the inferred type
--        asx :: forall e b.
--               MonadThrowable (StReader [(String, b)] e) =>
--               [DefFun] -> StReader [(String, b)] e ()  

run1 = f $ (runStReader $ asx [func, func2, func3, func4] []) [("print",typ)] []
      where f Nothing = error "Nothing"
            f (Just (s,_)) = map (\(n,t) -> n ++ " :: " ++ prettyPrinterType t) s

run2 = f $ (runStReader $ asx [func5] []) [("length",typ2)] []
      where f Nothing = error "Nothing"
            f (Just (s,_)) = map (\(n,t) -> n ++ " :: " ++ prettyPrinterType t) s


run func dts = f $ (runStReader $ asx func dts) [("invokeWS",callWSType)] []
      where f Nothing = error "Nothing"
            f (Just (s,_)) = map (\(n,t) -> n ++ " :: " ++ prettyPrinterType t) s

runL f d = run [f] [d]

r = runStReader

buildGraph funcs externFuncs = do edges <- foldM (kkp nodes $ externFuncs) (initEdges $ length names) funcs
                                  return $ makeGraph edges
                               where nodes = zip names [0..]   
                                     names = map funname funcs

graphTopSort graph = do guardT (not $ cyclicGraph $ graph) ()
                        return $ topSort graph

-- Se pasa externFuncs como parametro, en vez de obtenerlo mediante get, debido a que no se quiere obligar a los metodos
-- a usar explicitamente StReader (se usa cualquier instancia de MonadThrowable). 

--processFunctions :: (MonadThrowable m) => [DefFun] -> [(String,Type)] -> m [Type]
processFunctions funcs externFuncs = do analyzeFuncSyntax funcs
                                        graph <- buildGraph funcs (map fst externFuncs)
                                        vs <- graphTopSort graph
                                        mapM (\v -> typeChecking (funcs !! v)) vs

--typeChecking usa explicitamente StReader

processDataTypes dts = do analyzeDataTypesSyntax dts
                          return $ dts >>= dtype2Fun 

dtype2Fun dt = map (\(ctr, ty) -> (ctr, Rec (Single $ UserDefined $ typeName dt) ty)) $ constructors dt 
                                       
initEdges :: Int -> [(Int, [Int])]
initEdges 0 = []
initEdges n = (n - 1, []) : initEdges (n - 1)

doIt Nothing = error "Errror"
doIt (Just v) = makeGraph v

--kkp :: (MonadThrowable m) => [(String, Int)] -> [String] -> [(Int,[Int])] -> DefFun -> m [(Int,[Int])]
kkp xs externFuncs ys fun = do node <- lookupT (funname fun) xs
                               kexpr node . body $ fun
                where kexpr n (Call name' exprs) = guardT (elem name' $ paramFuncs ++ externFuncs) ys <|> 
                                                   (do dd <- lookupT name' xs
                                                       return $ addT ys dd n)  
                      kexpr n _ = return ys
                      paramFuncs = map fst $ funparameters fun                 

lookupT :: (MonadThrowable m, Eq a) => a -> [(a,b)] -> m b
lookupT _ [] = throw $ "Unable to find this key"
lookupT x ((y,z):ys) | x == y = return z
                     | otherwise = lookupT x ys


addT :: Eq a => [(a, [b])] -> a -> b -> [(a, [b])]
addT [] a b = [(a, [b])]
addT ((x,bs):ys) a b | x == a = [(x, b:bs)] ++ ys
                     | otherwise = (x,bs) : addT ys a b

lowerFirstChar :: String -> String
lowerFirstChar (a:as) = (toLower a):as

upperFirstChar :: String -> String
upperFirstChar (a:as) = (toUpper a):as


func = DefFun "sum" [("a", Single $ TInt), ("b", Single $ TInt), ("c", Single $ TString)] (Call "show" [Free "a", Free "b"])
func2 = DefFun "show" [("a", Single $ TInt), ("b", Single $ TInt)] (Call "print" [Free "a", Free "b"])
func3 = DefFun "exc" [("a", Single $ TInt), ("b", Single $ TString)] (Call "sum" [Free "a", Free "a"])
func4 = DefFun "apply" [("sum", Rec (Single TString) (Single TInt)), ("b", Single $ TString)] (Call "sum" [Free "b"])
typ = Rec (Single TInt) (Rec (Single TInt) (Single TString))

func5 = DefFun "len" [("a", TList $ T $ Single TInt)] (Call "length" [Free "a"])
typ2 = Rec (TList Empty) (Single TInt)
typ3 = Rec (Single TString) (Rec (Single TString) (Single TString))

callWSType = Rec (Single TString) $ Rec (Single TString) $ Rec (Single TString) $ Rec (Single TString) $ Rec (TList Empty) (Single TString)
