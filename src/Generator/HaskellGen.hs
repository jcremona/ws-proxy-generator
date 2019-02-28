module Generator.HaskellGen (write, Module, DefFun, DataType, runGenerationModule, defineFunction, datatype, typeName, fields, moduleName) where

import Generator.StReader
import Common.Util
import Control.Monad
import Generator.DGraph
import Control.Monad.Catch (MonadThrow)
import Generator.TypeChecking
import Data.Char
import Data.Text.Prettyprint.Doc
import Data.Text.Prettyprint.Doc.Render.Text
import Data.Text.IO
import Prelude hiding (writeFile)
---------------------------------------------------------------------------------
-- El módulo que se encarga de la generación de código Haskell
---------------------------------------------------------------------------------



---------------------------------------------------------------------------------
-- Permite definir records, funciones, y agruparlos en un módulo Haskell 
---------------------------------------------------------------------------------
data DataType = DataType 
              { typeName :: String
              , fields   :: [(String, Type)]
              } deriving Show

data DefFun = DefFun
            { funname       :: String
            , funparameters :: [(String, Type)]
            , body          :: Expr
            , funcType      :: Type
            } deriving Show

data Module = Module
            { moduleName :: String
            , functions  :: [(DefFun, DefFun)] -- Por cada Function del modelo intermedio, se definen dos DefFun (ver HaskellTranslate)
            , dataTypes  :: [DataType]
            } deriving Show

type TypedParams = [(String, Type)]

datatype = DataType
defineFunction = DefFun
defineModule = Module


write m = writeFile ("./" ++ moduleName m ++ ".hs") $ (renderStrict . layoutPretty defaultLayoutOptions) $ prettyPrinterModule m


-----------------------------------------
-- PRETTY PRINTER
-----------------------------------------
vspace = hardline <> hardline

---------------------------------------------------------------------------------------
-- prettyPrinterModule: Genera el código para un Module dado.
-- En HaskellTranslate, vimos que para cada Function se definen dos funciones.
-- Este método es más que un pretty printer, ya que para cada par de estas funciones,
-- genera una tercera, que invoca al par mencionado. Esto se hace así porque nuestro
-- AST (que representa la expresiones Haskell que podemos representar en una función)
-- no provee soporte para mónadas (notación do, bindings, return). Además hace un renombre,
-- de modo tal que la función principal (generada "artificialmente" aquí)
-- lleve el mismo nombre con el que fue definida en el WSDL.
---------------------------------------------------------------------------------------
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


-----------------------------------------
-- TYPE CHECKING
-- Llama al módulo TypeChecking.
-----------------------------------------

typeChecking (DefFun name params body funcType) = do bodyType <- local (const params) $ typeCheckingExpr body
                                                     funcs <- get
                                                     funcType' <- return $ foldr (\ (_,t) ty -> functionType t ty) bodyType params
                                                     if equalType funcType funcType' then 
                                                             (put $ (name, funcType):funcs) >> return funcType
                                                      else throwCustomExceptionM $ "Couldn't match type of the function " ++ name ++ " with the type of the body"


-----------------------------------------
-- SYNTAX ANALYZER 
-----------------------------------------

reservedWords = ["case","class","data","default","deriving","do","else","forall"
  ,"if","import","in","infix","infixl","infixr","instance","let","module"
  ,"newtype","of","qualified","then","type","where"
  ,"foreign","ccall","as","safe","unsafe"]


guardT :: (MonadThrow m) => Bool -> String -> b -> StReader m a c b
guardT condition errorMsg value = if condition then return value else throwCustomExceptionM errorMsg 

lower [] = throwCustomExceptionM "Invalid identifier (empty String)"
lower (t@(x:xs)) = guardT (isLower x) ("String must be lowercase: " ++ t) t

upper [] = throwCustomExceptionM "Invalid identifier (empty String)"
upper (t@(x:xs)) = guardT (isUpper x) ("String must be uppercase: " ++ t) t

notAReservedWord xs = guardT (notElem xs reservedWords) ("Reserved Word: " ++ xs) xs

validIdentifier :: (MonadThrow m) => String -> StReader m a c String
validIdentifier = (>>= notAReservedWord) . lower

allDifferent :: (Eq a) => [a] -> Bool
allDifferent []     = True
allDifferent (x:xs) = x `notElem` xs && allDifferent xs 

syntaxAnalyzer :: (MonadThrow m) => DefFun -> StReader m a c DefFun
syntaxAnalyzer fun = do fname <- validIdentifier . funname $ fun
                        ps <- guardT (allDifferent $ map fst params ++ [fname]) "Params and function must have different names" params
                        mapM (validIdentifier . fst) ps
                        return fun
                     where params = funparameters fun
                           

--analyzeFuncSyntax :: (MonadThrow m) =>[DefFun] -> StReader m s e ()
analyzeFuncSyntax funcs = do externFuncs <- get
                             let externFuncNames = map fst externFuncs
                             mapM syntaxAnalyzer funcs
                             mapM notAReservedWord externFuncNames
                             guardT (allDifferent $ map funname funcs ++ externFuncNames) "Functions must have different names" ()

dataTypeSyntaxAnalyzer :: (MonadThrow m) => DataType -> StReader m s e DataType
dataTypeSyntaxAnalyzer dt = do mapM (lower . fst) $ fields dt
                               upper $ typeName dt
                               return dt
            
analyzeDataTypesSyntax :: (MonadThrow m) => [DataType] ->  StReader m s e ()
analyzeDataTypesSyntax dts = do mapM dataTypeSyntaxAnalyzer dts
                                guardT (allDifferent $ map typeName dts) "Type names must have different names" ()


-------------------------------------------------------------
-- BUILT-IN FUNCTIONS
-- Signature de las funciones 
-- que se definen en el módulo Callws.hs
-- y que se utilizarán en "runtime" (cuando
-- se invoque el código generado.
-------------------------------------------------------------

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


-------------------------------------------
-------------------------------------------
 

--------------------------------------------------------------
-- runGenerationModule: Método que ejecuta las operaciones mónadicas (StReader)
-- para generar los módulos Haskell.
--------------------------------------------------------------
runGenerationModule moduleName funcs dts = runStReader (buildModule moduleName funcs dts) builtInFunctions [] >>= return . fst

analyze funcs dts = do externFuncs <- get
                       dtFuncs <- processDataTypes dts
                       put $ externFuncs ++ dtFuncs
                       processFunctions funcs --(externFuncs ++ dtFuncs)
                       --get -- FIXME borrar?

------------------------------------------------------------
-- processFunctions: analiza la sintaxis de las funciones,
-- construye un grafo dirigido, y realiza chequeo de tipos.
------------------------------------------------------------
processFunctions funcs = do externFuncs <- get
                            analyzeFuncSyntax funcs
                            graph <- buildGraph funcs (map fst externFuncs) 
                            vs <- graphTopSort graph
                            mapM (\v -> typeChecking (funcs !! v)) vs
                           

--------------------------------------------------------------------
-- processDataTypes: analiza la sintaxis de los datatypes 
-- definidos, y agrega los constructores a la lista de funciones.
--------------------------------------------------------------------
processDataTypes dts = do analyzeDataTypesSyntax dts
                          return $ (map constructor2Fun dts) ++ (dts >>= dtype2Fun) 

dtype2Fun dt = map (\(ctr, ty) -> (ctr, functionType ( userDefinedType $ typeName dt) ty)) $ fields dt 

constructor2Fun dt = (typeName dt, foldr (\(c,t) ty -> functionType t ty) (userDefinedType $ typeName dt) (fields dt))


buildModule moduleName funcs dts = do analyze (flatten funcs) dts
                                      return $ Module (upperFirstChar moduleName) funcs dts
                                    where flatten = (>>= (\(a,b) -> [a,b]))

--------------------------------------------------------------
-- Operaciones de grafos. Llama al módulo DGraph
--------------------------------------------------------------

buildGraph funcs externFuncs = do edges <- foldM (makeEdges nodes $ externFuncs) (initEdges $ length names) funcs
                                  return $ makeGraph edges
                               where nodes = zip names [0..]   
                                     names = map funname funcs


--------------------------------------------------------------
-- graphTopSort: ordenamiento topológico
--------------------------------------------------------------
graphTopSort graph = do guardT (not $ cyclicGraph $ graph) "Graph error" ()
                        return $ topSort graph
                                       
initEdges :: Int -> [(Int, [Int])]
initEdges 0 = []
initEdges n = (n - 1, []) : initEdges (n - 1)


--------------------------------------------------------------
-- makeEdges: construye las aristas del grafo, 
-- a partir de analizar los cuerpos de las funciones.
-- Si encuentra una llamada a una función g en el cuerpo de una
-- función f, agrega una arista de g a f.
-- Las excepciones son llamadas a funciones externas, 
-- o pasadas por parámetro.
--------------------------------------------------------------
makeEdges
  :: (MonadThrow m, Eq a) =>
     [(String, a)] -> [String] -> [(a, [a])] -> DefFun -> StReader m s e [(a, [a])]
makeEdges nodes externFuncs edges fun = do node <- lookupT (funname fun) nodes
                                           lookForCall node edges . body $ fun
                where lookForCall n edges (Call_ name' exprs) = if elem name' $ paramFuncs ++ externFuncs then return edges else 
                                                               (do dd <- lookupT name' nodes
                                                                   return $ addT edges dd n)
                      lookForCall n edges (List_ exprs) = foldM (lookForCall n) edges exprs  
                      lookForCall n edges (Tuple_ (e1,e2)) = do edges' <- lookForCall n edges e1
                                                                lookForCall n edges' e2
                      lookForCall n edges _ = return edges
                      paramFuncs = map fst $ funparameters fun                 

lookupT x [] = throwCustomExceptionM $ "Unable to find this key: " ++ x
lookupT x ((y,z):ys) | x == y = return z
                     | otherwise = lookupT x ys


addT :: Eq a => [(a, [b])] -> a -> b -> [(a, [b])]
addT [] a b = [(a, [b])]
addT ((x,bs):ys) a b | x == a = [(x, b:bs)] ++ ys
                     | otherwise = (x,bs) : addT ys a b

