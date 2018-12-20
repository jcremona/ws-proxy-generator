{-# LANGUAGE RankNTypes                #-}

module Generator.Gen where

import Generator.StReader
import Data.Char
import Control.Monad
import Generator.DGraph
import Control.Monad.Catch (MonadThrow)
import Generator.TypeChecking
import Data.Text.Prettyprint.Doc
import Data.Text.Prettyprint.Doc.Render.Text
import Data.Text.IO
import Prelude hiding (writeFile)

data DataType = DataType 
              { typeName :: String
              , fields   :: [(String, Type)]
              } deriving Show

data DefFun = DefFun
            { funname       :: String
            , funparameters :: [(String, Type)] -- un param puede ser una funcion, deberia poder usarse esa funcion localmente
            , body          :: Expr
            , funcType      :: Type
            } deriving Show

data Module = Module
            { moduleName :: String
            , functions  :: [(DefFun, DefFun)]
            , dataTypes  :: [DataType]
            } deriving Show


type TypedParams = [(String, Type)]

write m = writeFile ("./" ++ moduleName m ++ ".hs") $ (renderStrict . layoutPretty defaultLayoutOptions) $ prettyPrinterModule m

vspace = hardline <> hardline


prettyPrinterModule :: Module -> Doc ann
prettyPrinterModule (Module name funcs dts) = pretty "module" <+> pretty name <+> pretty "where" 
                                             <> vspace 
                                             <> pretty "import Callws" <> hardline <> pretty "import Control.Monad" 
                                             <> vspace 
                                             <> sep (map ((<> hardline) . prettyPrinterDataType) dts) 
                                             <> hardline 
                                             <> sep (map (\(f,auxF) -> prettyPrinterFun (f {funname = primeFunName f}) 
                                                                       <> vspace 
                                                                       <> prettyPrinterFun auxF 
                                                                       <> vspace 
                                                                       <> runFunction (funname f) (primeFunName f) (funname auxF)) funcs) <> line

                                         where runFunction thisname name auxName = pretty thisname <+> equals <+> parens (pretty "liftM" <+> pretty auxName) <+> dot <+> pretty name
                                               primeFunName f = funname f ++ "'"   

prettyPrinterFun :: DefFun -> Doc ann
prettyPrinterFun fun = pretty (funname fun) <+> pretty "::" <+> prettyPrinterType (funcType fun) <> hardline <> pretty (funname fun) <+> prettyPrinterParam (funparameters fun) <+> equals <+> prettyPrinterExpr (body fun)  

prettyPrinterDataType :: DataType -> Doc ann
prettyPrinterDataType (DataType name fields) = pretty "data" <+> pretty name <+> equals <+> pretty name <+> braces ( align . vsep  $ punctuate comma (map prettyPrinterField fields)) <+> pretty "deriving (Show)"
                                                where prettyPrinterField (n, t) = pretty n <+> pretty "::" <+> prettyPrinterType t 

prettyPrinterParam :: [(String, Type)] -> Doc ann
prettyPrinterParam xs = sep $ map (pretty . fst) xs

--typeChecking :: DefFun -> StReader [(String, Type)] TypedParams Type
typeChecking (DefFun name params body funcType) = do bodyType <- local (const params) $ typeCheckingExpr body
                                                     funcs <- get
                                                     funcType' <- return $ foldr (\ (_,t) ty -> functionType t ty) bodyType params
                                                     if equalType funcType funcType' then 
                                                             (put $ (name, funcType):funcs) >> return funcType
                                                      else throwCustomExceptionM $ "Couldn't match type of the function " ++ name ++ " with the type of the body"

reservedWords = ["case","class","data","default","deriving","do","else","forall"
  ,"if","import","in","infix","infixl","infixr","instance","let","module"
  ,"newtype","of","qualified","then","type","where"
  ,"foreign","ccall","as","safe","unsafe"]

--lookupVble :: Eq key => key -> StReader s [(key, value)] value


guardT :: (MonadThrow m) => Bool -> String -> b -> InternalStReader m a c b
guardT condition errorMsg value = if condition then return value else throwCustomExceptionM errorMsg 


--lower :: (MonadThrowable m) => String -> m String
lower [] = throwCustomExceptionM "Invalid identifier (empty String)"
lower (t@(x:xs)) = guardT (isLower x) ("String must be lowercase: " ++ t) t

--upper :: (MonadThrowable m) => String -> m String
upper [] = throwCustomExceptionM "Invalid identifier (empty String)"
upper (t@(x:xs)) = guardT (isUpper x) ("String must be uppercase: " ++ t) t

--notAReservedWord :: (MonadThrowable m) => String -> m String
notAReservedWord xs = guardT (notElem xs reservedWords) ("Reserved Word: " ++ xs) xs

--validIdentifier :: (MonadThrowable m) => String -> m String
validIdentifier :: (MonadThrow m) => String -> InternalStReader m a c String
validIdentifier = (>>= notAReservedWord) . lower

allDifferent :: (Eq a) => [a] -> Bool
allDifferent []     = True
allDifferent (x:xs) = x `notElem` xs && allDifferent xs 

syntaxAnalyzer :: (MonadThrow m) => DefFun -> InternalStReader m a c DefFun
syntaxAnalyzer fun = do fname <- validIdentifier . funname $ fun
                        ps <- guardT (allDifferent $ map fst params ++ [fname]) "Params and function must have different names" params
                        mapM (validIdentifier . fst) ps
                        return fun
                     where params = funparameters fun
                           

analyzeFuncSyntax :: (MonadThrow m) =>[DefFun] -> InternalStReader m s e ()
analyzeFuncSyntax funcs = do mapM syntaxAnalyzer funcs
                             guardT (allDifferent $ map funname funcs) "Functions must have different names" ()

dataTypeSyntaxAnalyzer :: (MonadThrow m) => DataType -> InternalStReader m s e DataType
dataTypeSyntaxAnalyzer dt = do mapM (lower . fst) $ fields dt
                               upper $ typeName dt
                               return dt
            
analyzeDataTypesSyntax :: (MonadThrow m) => [DataType] ->  InternalStReader m s e ()
analyzeDataTypesSyntax dts = do mapM dataTypeSyntaxAnalyzer dts
                                guardT (allDifferent $ map typeName dts) "Type names must have different names" ()


analyze funcs dts = do externFuncs <- get
                       dtFuncs <- processDataTypes dts
                       put $ externFuncs ++ dtFuncs
                       processFunctions funcs (externFuncs ++ dtFuncs)
                       get 
                       

processFunctions funcs externFuncs = do analyzeFuncSyntax funcs
                                        graph <- buildGraph funcs (map fst externFuncs)
                                        vs <- graphTopSort graph
                                        mapM (\v -> typeChecking (funcs !! v)) vs

processDataTypes dts = do analyzeDataTypesSyntax dts
                          return $ (map constructor2Fun dts) ++ (dts >>= dtype2Fun) 

dtype2Fun dt = map (\(ctr, ty) -> (ctr, functionType ( userDefinedType $ typeName dt) ty)) $ fields dt 

constructor2Fun dt = (typeName dt, foldr (\(c,t) ty -> functionType t ty) (userDefinedType $ typeName dt) (fields dt))


buildModule moduleName funcs dts = do analyze (flatten funcs) dts
                                      return $ Module (upperFirstChar moduleName) funcs dts

flatten = (>>= (\(a,b) -> [a,b]))

showIntType = functionType intType stringType
showDoubleType = functionType doubleType stringType
showLongType = functionType longType stringType
showCharType = functionType charType stringType
showFloatType = functionType floatType stringType
showBoolType = functionType boolType stringType
showVoidType = functionType voidType stringType

readIntType = functionType stringType intType 
readDoubleType = functionType stringType doubleType
readLongType = functionType stringType longType 
readCharType = functionType stringType charType
readFloatType = functionType stringType floatType
readBoolType = functionType stringType boolType
readVoidType = functionType stringType voidType

takeStringType = functionType (listType stringType) $ functionType intType stringType 

callWSType = functionType stringType $ functionType stringType $ functionType stringType $ functionType (listType $ tupleType (stringType, stringType)) $ functionType stringType $ functionType (listType stringType) (ioMonadType $ listType stringType)

builtInFunctions = [("invokeWS",callWSType),("showInt",showIntType),("showDouble",showDoubleType),("showLong",showLongType),("showChar",showCharType),("showFloat",showFloatType),("showBool",showBoolType),("showVoid",showVoidType),("readInt",readIntType),("readDouble",readDoubleType),("readLong",readLongType),("readChar",readCharType),("readFloat",readFloatType),("readBool",readBoolType),("readVoid",readVoidType),("takeString",takeStringType)]
 
runGenerationModule moduleName funcs dts = runStReader (buildModule moduleName funcs dts) builtInFunctions [] >>= return . fst


-------------------------------------------------------------------------------------------------------------
-------------------------------------------------------------------------------------------------------------

buildGraph funcs externFuncs = do edges <- foldM (kkp nodes $ externFuncs) (initEdges $ length names) funcs
                                  return $ makeGraph edges
                               where nodes = zip names [0..]   
                                     names = map funname funcs

graphTopSort graph = do guardT (not $ cyclicGraph $ graph) "Graph error" ()
                        return $ topSort graph
                                       
initEdges :: Int -> [(Int, [Int])]
initEdges 0 = []
initEdges n = (n - 1, []) : initEdges (n - 1)

--kkp :: (MonadThrowable m) => [(String, Int)] -> [String] -> [(Int,[Int])] -> DefFun -> m [(Int,[Int])]
kkp xs externFuncs ys fun = do node <- lookupT (funname fun) xs
                               kexpr node . body $ fun
                where kexpr n (Call_ name' exprs) = if elem name' $ paramFuncs ++ externFuncs then return ys else 
                                                   (do dd <- lookupT name' xs
                                                       return $ addT ys dd n)  
                      kexpr n _ = return ys
                      paramFuncs = map fst $ funparameters fun                 

--lookupT :: a -> [(a,b)] -> m b
lookupT x [] = throwCustomExceptionM $ "Unable to find this key: " ++ x
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


--run1 = f $ (runStReader $ asx [func, func2, func3, func4] []) [("print",typ)] []
--      where f Nothing = error "Nothing"
--            f (Just (s,_)) = map (\(n,t) -> n ++ " :: " ++ prettyPrinterType t) s


--run2 :: [String]--([(String, Type)], [(String, Type)])
--run2 = f $ (runStReader $ analyze [func5] []) [("length",typ2)] []
--      where f (Left e) = [show e]
--            f (Right (s,_)) = map (\(n,t) -> n ++ " :: " ++ prettyPrinterType t) s



--run func dts = f $ (runStReader $ asx func dts) [("invokeWS",callWSType)] []
--      where f Nothing = error "Nothing"
--            f (Just (s,_)) = map (\(n,t) -> n ++ " :: " ++ prettyPrinterType t) s

--runL f d = run [f] [d]


--func = DefFun "sum" [("a",  $ intType), ("b",  $ intType), ("c",  $ stringType)] (Call "show" [Free "a", Free "b"])
--func2 = DefFun "show" [("a",  $ intType), ("b",  $ intType)] (Call "print" [Free "a", Free "b"])
--func3 = DefFun "exc" [("a",  $ intType), ("b",  $ stringType)] (Call "sum" [Free "a", Free "a"])
--func4 = DefFun "apply" [("sum", functionType ( stringType) ( intType)), ("b",  $ stringType)] (Call "sum" [Free "b"])
--typ = functionType ( intType) (functionType ( intType) ( stringType))

-- TODO para este ejemplo eq (Empty) (T ty) debe dar false, pero para el de abajo debe dar True! hay que distinguir entre la lista vacia y un tipo de Lista Parametrico 
--func5 = DefFun "len" [("a", TList EmptyListType)] (Call "length" [Free "a"]) (functionType (TList EmptyListType) ( intType))
--typ2 = functionType (TList $ T $  intType) ( intType)

--func5 = DefFun "len" [] (Call "length" []) (functionType (TList EmptyListType) ( intType))
typ2 = functionType (listType intType) ( intType)


func5 = DefFun "len" [("xs", stringType)] (listExpr []) (functionType stringType $ listType $ listType intType)


len :: String -> [[Int]]
len s = [[]]
--typ3 = functionType ( stringType) (functionType ( stringType) ( stringType))

--len :: [a] -> Int
--len a = lt a

--lt :: [Int] -> Int
--lt = const 2


