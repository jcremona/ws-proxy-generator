{-# LANGUAGE TemplateHaskell #-}
module Model.Translate (translate, Module, write, moduleName) where

import Generator.Gen
import Model.WSDL2Model
import Data.Text hiding (map, head,zip)
import Data.XML.Types
import Model.ProxyModel
import Text.XML.WSDL.Parser
import Data.ByteString.Lazy (fromStrict)
import Network.URI
import Control.Monad.Catch (MonadThrow)
import Control.Exception
import Common.Exception
import Control.Monad.Reader
import Text.XML.WSDL.Types hiding (types)
import qualified Data.ByteString as B
import Data.Text.Encoding (encodeUtf8)
import Generator.TypeChecking

portsToModules :: URI -> [Params] -> [(Port, URI)] -> Either SomeException [Module]
portsToModules namespace params = mapM (\(port, uri) -> runGenerationModule (unpack $ bName port) (bindingsToFunDefinitions namespace uri $ protocolBinding port) datatypes)
                                 where datatypes = map params2DataType params

bindingsToFunDefinitions :: URI -> URI -> [ProtocolBinding] -> [(DefFun, DefFun)]
bindingsToFunDefinitions namespace address = map ((\fun -> (toFunctionDefinition namespace address fun, auxiliarFunctionDefinition fun)) . pFunction)

toFunctionDefinition :: Network.URI.URI -> Network.URI.URI -> Function -> DefFun
toFunctionDefinition  namespace address (Function functionName params funcType _) =  DefFun (lowerFirstChar fname) [("dt", inputType)] 
                                                                     (callExpr "invokeWS" [stringExpr $ show address, 
                                                                                       stringExpr fname,
                                                                                       stringExpr $ show namespace, 
                                                                                       listExpr $ map (\(c,t) -> tupleExpr(stringExpr c, convertToString t $ callExpr c [freeVbleExpr "dt"])) $ fields dt, 
                                                                                       stringExpr return, 
                                                                                       listExpr responseParameterNames]) (functionType inputType $ ioMonadType $ listType stringType)
                                                           where dt = params2DataType $ messageType params
                                                                 fname = unpack functionName
                                                                 inputType = userDefinedType . typeName $ dt
                                                                 return = unpack . wrapperName . messageType $ funcType
                                                                 responseParameterNames = map (stringExpr . unpack . nameLocalName . parameterName) (parameters . messageType $ funcType)


auxiliarFunctionDefinition (Function functionName params funcType _) = DefFun ("buildOutput_" ++ fname) [("xs", listType stringType)]
                                                                        (callExpr (typeName retDataType) args) (functionType (listType stringType) (userDefinedType $ typeName retDataType))
                                                           where fname = unpack functionName
                                                                 retDataType = params2DataType $ messageType funcType
                                                                 --return = unpack . wrapperName . messageType $ funcType
                                                                 args = [convertFromString t $ callExpr "takeString" [freeVbleExpr "xs", intExpr i] | (i,(c,t))  <- enumerate $ fields retDataType]
                                                                 enumerate = zip [0..]
                                                                 --responseParameterNames = map (stringExpr . unpack . nameLocalName . parameterName) (parameters . messageType $ returnType)

 -- FIXME el param dt no puede ser igual a ninguna funcion??


params2DataType :: Params -> DataType
params2DataType (Params name parameters) = DataType (upperFirstChar . unpack $ name) $ map parameter2Constructor parameters 

parameter2Constructor :: Parameter -> (String, Type)
parameter2Constructor (Parameter (Name nl ns pr) ty) = (lowerFirstChar . unpack $ nl, wsType2Type ty)

wsType2Type (WSPrimitiveType WSString) = stringType
wsType2Type (WSPrimitiveType WSInt) = intType
wsType2Type (WSPrimitiveType WSDouble) = doubleType
wsType2Type (WSPrimitiveType WSLong) = longType
wsType2Type (WSPrimitiveType WSChar) = charType
wsType2Type (WSPrimitiveType WSFloat) = floatType
wsType2Type (WSPrimitiveType WSVoid) = voidType
wsType2Type (WSPrimitiveType WSBoolean) = boolType  

packStr'' :: String -> B.ByteString
packStr'' = encodeUtf8 . pack

parsedWsdl :: (MonadThrow m) => String -> m WSDL
parsedWsdl = parseLBS . fromStrict . packStr''

imodel :: (MonadThrow m) => String -> m WSAbstraction
imodel wsdlFile = parsedWsdl wsdlFile >>= (runReader buildModel)

maybeToEither = flip maybe Right . Left

translate :: String -> Either SomeException [[Module]]
translate wsdlFile = do model <- imodel wsdlFile
                        nspace <- maybeToEither (throwCustomException "Undefined Namespace") $ namespace model
                        mapM ((portsToModules nspace $ types model) . ports) (defServices model)

