{-# LANGUAGE PatternSynonyms #-}
module Generator.TypeChecking 
(
  Type,
  Expr,
  intType,
  stringType,
  doubleType,
  longType,
  charType,
  floatType,
  voidType,
  boolType,
  functionType,
  listType,
  tupleType,
  ioMonadType,
  userDefinedType,

  callExpr,
  freeVbleExpr,
  stringExpr,
  listExpr,
  tupleExpr,
  intExpr,
  typeCheckingExpr,

  prettyPrinterExpr,
  prettyPrinterType,

  equalType,

  convertToString,
  convertFromString,

  pattern Call_,
  pattern List_,
  pattern Tuple_,
) where

import Generator.StReader
import Control.Monad
import Control.Applicative
import Data.Text.Prettyprint.Doc


---------------------------------------------------------------------------
-- Permite definir las expresiones que pueden aparecer 
-- en el cuerpo de una función definida.
 ---------------------------------------------------------------------------
data Expr = Call String [Expr] | Free String | StringValue String | ListValue [Expr] | TupleValue (Expr,Expr) | IntValue Int deriving Show


---------------------------------------------------------------------------
-- Los tipos a utilizar durante el chequeo de tipos.
---------------------------------------------------------------------------
data Type = Func Type Type | TList ListType | TTuple (Type, Type) | IOMonad Type | UserDefined String | TInt | TString | TDouble | TLong | TChar | TFloat | TVoid | TBool deriving (Show)
data ListType = EmptyListType | T Type deriving (Show)

-----------------------------------------------------------------------------------------
-- El código de este módulo es bastante simple, pero vale destacar el
-- caso excepcional de la lista vacía. Durante el chequeo de tipos,
-- a la lista vacía se le asigna el tipo especial TList EmptyListType.
-- Esto es porque al procesar la lista vacía sin contexto, no corresponde asignarle
-- ningún tipo de lista en particular. Luego, en equalType, este tipo particular
-- matchea con cualquier tipo de lista concreto 
-- (es decir, lista de Int, lista de String, etc.)
-- Por fuera de este módulo, el cliente no puede asignar a ninguna expresión el
-- tipo TList EmptyListType, porque los constructores así no lo permiten (ver listType).
-----------------------------------------------------------------------------------------

intType = TInt
stringType = TString
doubleType = TDouble
longType = TLong
charType = TChar
floatType = TFloat
voidType = TVoid
boolType = TBool

functionType = Func
listType = TList . T
tupleType = TTuple
ioMonadType = IOMonad
userDefinedType = UserDefined


callExpr = Call
freeVbleExpr = Free
stringExpr = StringValue
listExpr = ListValue
tupleExpr = TupleValue
intExpr = IntValue

prettyPrinterType :: Type -> Doc ann
prettyPrinterType (Func (t@(Func _ _)) ttype') = parens (prettyPrinterType t) <+> pretty "->" <+> prettyPrinterType ttype'
prettyPrinterType (Func ttype ttype') = prettyPrinterType ttype <+> pretty "->" <+> prettyPrinterType ttype'
prettyPrinterType (TList EmptyListType) = brackets $ pretty "a" -- JUST for debug purposes
prettyPrinterType (TList (T ttype)) = brackets $ prettyPrinterType ttype
prettyPrinterType (TTuple (ttype, ttype')) = tupled [prettyPrinterType ttype, prettyPrinterType ttype']
prettyPrinterType (IOMonad ttype) = pretty "IO " <+> parens (prettyPrinterType ttype)
 
prettyPrinterType (UserDefined udtype) = pretty udtype
prettyPrinterType TInt = pretty "Int"
prettyPrinterType TString = pretty "String"
prettyPrinterType TDouble = pretty "Double"
prettyPrinterType TLong = pretty "Integer"
prettyPrinterType TChar = pretty "Char"
prettyPrinterType TFloat = pretty "Float"
prettyPrinterType TVoid = pretty "()"
prettyPrinterType TBool = pretty "Bool"

prettyPrinterExpr :: Expr -> Doc ann
prettyPrinterExpr (Call name []) = pretty name
prettyPrinterExpr (Call name xs) = pretty name <+> (align . sep $ (map auxPrettyPrinterExpr xs))
                                 where auxPrettyPrinterExpr (e@(Call _ (x:xs))) = parens $ prettyPrinterExpr e
                                       auxPrettyPrinterExpr expr = prettyPrinterExpr expr 
prettyPrinterExpr (Free vble) = pretty vble  
prettyPrinterExpr (StringValue str) = dquotes $ pretty str
prettyPrinterExpr (IntValue int) = pretty int
prettyPrinterExpr (ListValue ls) = list $ map prettyPrinterExpr ls
prettyPrinterExpr (TupleValue (e1,e2)) = tupled [prettyPrinterExpr e1, prettyPrinterExpr e2]



pattern Call_ name exprs <- Call name exprs where
     Call_ n xs = Call n xs

pattern List_ exprs <- ListValue exprs where
     List_ xs = ListValue xs

pattern Tuple_ exprs <- TupleValue exprs where
     Tuple_ (e1,e2) = TupleValue (e1,e2)

typeCheckingExpr (Call name subexprs) = do params <- ask
                                           funcs <- get
                                           fn $ lookup name params <|> lookup name funcs 
                                          where fn Nothing = throwCustomExceptionM $ "Undefined function: " ++ name      
                                                fn (Just t) = checkCallExprs subexprs t 
typeCheckingExpr (Free vble) =  lookupVble vble
typeCheckingExpr (StringValue _) = return stringType
typeCheckingExpr (IntValue _) = return intType
typeCheckingExpr (ListValue exprs) = foldM (\ res  expr -> do t1 <- typeCheckingExpr expr
                                                              if equalType (listType t1) res 
                                                               then return $ listType t1
                                                               else throwCustomExceptionM "Malformed list: elements with different types")
                                           (TList EmptyListType) 
                                           exprs  
typeCheckingExpr (TupleValue (e1, e2)) = do t1 <- typeCheckingExpr e1
                                            t2 <- typeCheckingExpr e2
                                            return $ tupleType (t1, t2)


convertToString :: Type -> Expr -> Expr
convertToString TString expr = expr
convertToString TInt expr = callExpr "showInt" [expr]  
convertToString TDouble expr = callExpr "showDouble" [expr]
convertToString TLong expr = callExpr "showLong" [expr]
convertToString TChar expr = callExpr "showChar" [expr]
convertToString TFloat expr = callExpr "showFloat" [expr]
convertToString TBool expr = callExpr "showBool" [expr]
convertToString TVoid expr = callExpr "showVoid" [expr]
convertToString _ expr = expr

convertFromString :: Type -> Expr -> Expr
convertFromString TString expr = expr
convertFromString TInt expr = callExpr "readInt" [expr]  
convertFromString TDouble expr = callExpr "readDouble" [expr]
convertFromString TLong expr = callExpr "readLong" [expr]
convertFromString TChar expr = callExpr "readChar" [expr]
convertFromString TFloat expr = callExpr "readFloat" [expr]
convertFromString TBool expr = callExpr "readBool" [expr]
convertFromString TVoid expr = callExpr "readVoid" [expr]
convertFromString _ expr = expr

checkCallExprs [] t = return t 
checkCallExprs (exp:exps) (Func ttype ttype') = do ty <- typeCheckingExpr exp
                                                   if equalType ty ttype then checkCallExprs exps ttype' else throwCustomExceptionM "Function type doesn't match with parameters"    
checkCallExprs _ _ = throwCustomExceptionM "Function type doesn't match with parameters"

equalType TInt TInt = True
equalType TString TString = True
equalType TDouble TDouble = True
equalType TLong TLong = True
equalType TChar TChar = True
equalType TFloat TFloat = True
equalType TVoid TVoid = True
equalType TBool TBool = True
equalType (TList EmptyListType) (TList (T _)) = True
equalType (TList _) (TList EmptyListType) = True
equalType (TList (T t1)) (TList (T t2)) = equalType t1 t2
equalType (Func t1 t2) (Func t1' t2') = equalType t1 t1' && equalType t2 t2'
equalType (TTuple (t1, t2)) (TTuple (t1', t2')) = equalType t1 t1' && equalType t2 t2'
equalType (IOMonad t) (IOMonad t') = equalType t t'
equalType (UserDefined t) (UserDefined t') = t == t'
equalType t1 t2 = False


lookupVble vble = do params <- ask
                     case lookup vble params of                  
                         Nothing -> throwCustomExceptionM "Couldn't find parameter" 
                         Just t -> return t
