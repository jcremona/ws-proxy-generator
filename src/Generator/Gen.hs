module Generator.Gen (write, Module, DefFun, DataType, runGenerationModule, defineFunction, datatype, typeName, fields, moduleName) where

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
            , functions  :: [(DefFun, DefFun)]
            , dataTypes  :: [DataType]
            } deriving Show

type TypedParams = [(String, Type)]

datatype = DataType
defineFunction = DefFun
defineModule = Module


write m = writeFile ("./" ++ moduleName m ++ ".hs") $ (renderStrict . layoutPretty defaultLayoutOptions) $ prettyPrinterModule m


-- ************************************************************************************************************************************* --
-- *****************************************               PRETTY PRINTER               ************************************************ --
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


-- ************************************************************************************************************************************* --
-- ****************************************          TYPE CHECKING           *********************************************************** --

typeChecking (DefFun name params body funcType) = do bodyType <- local (const params) $ typeCheckingExpr body
                                                     funcs <- get
                                                     funcType' <- return $ foldr (\ (_,t) ty -> functionType t ty) bodyType params
                                                     if equalType funcType funcType' then 
                                                             (put $ (name, funcType):funcs) >> return funcType
                                                      else throwCustomExceptionM $ "Couldn't match type of the function " ++ name ++ " with the type of the body"


-- ************************************************************************************************************************************* --
-- ****************************************          SYNTAX ANALYZER         *********************************************************** --

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
                           

analyzeFuncSyntax :: (MonadThrow m) =>[DefFun] -> StReader m s e ()
analyzeFuncSyntax funcs = do mapM syntaxAnalyzer funcs
                             guardT (allDifferent $ map funname funcs) "Functions must have different names" ()

dataTypeSyntaxAnalyzer :: (MonadThrow m) => DataType -> StReader m s e DataType
dataTypeSyntaxAnalyzer dt = do mapM (lower . fst) $ fields dt
                               upper $ typeName dt
                               return dt
            
analyzeDataTypesSyntax :: (MonadThrow m) => [DataType] ->  StReader m s e ()
analyzeDataTypesSyntax dts = do mapM dataTypeSyntaxAnalyzer dts
                                guardT (allDifferent $ map typeName dts) "Type names must have different names" ()


-- ************************************************************************************************************************************* --
-- ****************************************             BUILT-IN FUNCTIONS         ***************************************************** --

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


-- ************************************************************************************************************************************* --
-- ************************************************************************************************************************************* --
 
runGenerationModule moduleName funcs dts = runStReader (buildModule moduleName funcs dts) builtInFunctions [] >>= return . fst

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
                                    where flatten = (>>= (\(a,b) -> [a,b]))

buildGraph funcs externFuncs = do edges <- foldM (kkp nodes $ externFuncs) (initEdges $ length names) funcs
                                  return $ makeGraph edges
                               where nodes = zip names [0..]   
                                     names = map funname funcs

graphTopSort graph = do guardT (not $ cyclicGraph $ graph) "Graph error" ()
                        return $ topSort graph
                                       
initEdges :: Int -> [(Int, [Int])]
initEdges 0 = []
initEdges n = (n - 1, []) : initEdges (n - 1)

kkp xs externFuncs ys fun = do node <- lookupT (funname fun) xs
                               kexpr node ys . body $ fun
                where kexpr n ys (Call_ name' exprs) = if elem name' $ paramFuncs ++ externFuncs then return ys else 
                                                      (do dd <- lookupT name' xs
                                                          return $ addT ys dd n)
                      kexpr n ys (List_ exprs) = foldM (kexpr n) ys exprs  
                      kexpr n ys (Tuple_ (e1,e2)) = do ys' <- kexpr n ys e1
                                                       kexpr n ys' e2
                      kexpr n ys _ = return ys
                      paramFuncs = map fst $ funparameters fun                 

lookupT x [] = throwCustomExceptionM $ "Unable to find this key: " ++ x
lookupT x ((y,z):ys) | x == y = return z
                     | otherwise = lookupT x ys


addT :: Eq a => [(a, [b])] -> a -> b -> [(a, [b])]
addT [] a b = [(a, [b])]
addT ((x,bs):ys) a b | x == a = [(x, b:bs)] ++ ys
                     | otherwise = (x,bs) : addT ys a b


--run2 :: [String]--([(String, Type)], [(String, Type)])
--run2 = f $ (runStReader $ analyze [func6, func5] []) [] []
--      where f (Left e) = [show e]
--            f (Right (s,_)) = map (\(n,t) -> n ++ " :: " ++ (show $ prettyPrinterType t)) s

--func5 = DefFun "len" [] (listExpr [callExpr "length" [listExpr []]]) (listType intType)
--func6 = DefFun "length" [("xs", listType intType)] (intExpr 2) typ2
--typ2 = functionType (listType intType) ( intType)
