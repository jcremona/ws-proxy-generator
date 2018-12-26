{-# LANGUAGE FlexibleInstances #-}
module Model.HaskellTranslate (HaskellCode) where

import Generator.HaskellGen
import Generator.TypeChecking
import Model.ProxyModel
import Model.CodeWriting
import Common.Exception
import Common.Util

import Data.Text hiding (map, head,zip)
import Data.XML.Types
import Network.URI
import Control.Monad.Catch (MonadThrow, throwM)

------------------------------------------------------------------
-- Los módulos HaskellGen y HaskellTranslate son dependientes
-- del lenguaje target elegido, en nuestro caso, Haskell.
-- Nos vemos forzados a implementar writeCode y buildFromModel. 
------------------------------------------------------------------

data HaskellCode = HaskellCode { modules :: [[Module]]} 

instance CodeWriting HaskellCode where
    writeCode = writeModuleLists . modules

instance (MonadThrow m) => TranslateFromModel (m HaskellCode) where
    buildFromModel = haskellModules

writeModuleList ms = mapM (\module_ -> write module_ >> putStrLn ("Module Generated: " ++ (moduleName module_))) ms
             
writeModuleLists mss = do mapM writeModuleList mss
                          return ()

haskellModules :: MonadThrow m => WSAbstraction -> m HaskellCode
haskellModules model = do nspace <- maybeToMonadThrow $ namespace model
                          modules <- mapM ((portsToModules nspace $ types model) . ports) (defServices model)
                          return $ HaskellCode modules
                        where maybeToMonadThrow = maybe (throwM $ throwCustomException "Undefined Namespace") return

portsToModules :: (MonadThrow m) => URI -> [Message] -> [(Port, URI)] -> m [Module]
portsToModules namespace params = mapM (\(port, uri) -> runGenerationModule (unpack $ bName port) (bindingsToFunDefinitions namespace uri $ protocolBinding port) datatypes)
                                 where datatypes = map message2DataType params


--------------------------------------------------------------------------------
-- Por cada Function definida en el modelo intermedio, se definen dos DefFun
-- Una función principal, que invoca a invokeWS, que es quien realiza 
-- la comunicación con el web service y está definido en Callws.
-- Y una función auxiliar para leer la respuesta obtenida (lista de Strings),
-- y mapearla al record correspondiente. Luego en HaskellGen, en la generación
-- de código, se genera "artificialmente" una tercera función que invoca al par.
--------------------------------------------------------------------------------

bindingsToFunDefinitions :: URI -> URI -> [ProtocolBinding] -> [(DefFun, DefFun)]
bindingsToFunDefinitions namespace address = map ((\fun -> (toFunctionDefinition namespace address fun, auxiliarFunctionDefinition fun)) . pFunction)

toFunctionDefinition :: Network.URI.URI -> Network.URI.URI -> Function -> DefFun
toFunctionDefinition  namespace address (Function functionName params funcType _) =  defineFunction (lowerFirstChar fname) [("dt", inputType)] 
                                                                     (callExpr "invokeWS" [stringExpr $ show address, 
                                                                                       stringExpr fname,
                                                                                       stringExpr $ show namespace, 
                                                                                       listExpr $ map (\(c,t) -> tupleExpr(stringExpr c, convertToString t $ callExpr c [freeVbleExpr "dt"])) $ fields dt, 
                                                                                       stringExpr return, 
                                                                                       listExpr responseParameterNames]) (functionType inputType $ ioMonadType $ listType stringType)
                                                           where dt = message2DataType $ messageType params
                                                                 fname = unpack functionName
                                                                 inputType = userDefinedType . typeName $ dt
                                                                 return = unpack . wrapperName . messageType $ funcType
                                                                 responseParameterNames = map (stringExpr . unpack . nameLocalName . parameterName) (parameters . messageType $ funcType)


auxiliarFunctionDefinition (Function functionName params funcType _) = defineFunction ("buildOutput_" ++ fname) [("xs", listType stringType)]
                                                                        (callExpr (typeName retDataType) args) (functionType (listType stringType) (userDefinedType $ typeName retDataType))
                                                           where fname = unpack functionName
                                                                 retDataType = message2DataType $ messageType funcType                                        
                                                                 args = [convertFromString t $ callExpr "takeString" [freeVbleExpr "xs", intExpr i] | (i,(c,t))  <- enumerate $ fields retDataType]
                                                                 enumerate = zip [0..]

 -- FIXME el param dt no puede ser igual a ninguna funcion??


message2DataType :: Message -> DataType
message2DataType (Message name parameters) = datatype (upperFirstChar . unpack $ name) $ map parameter2Constructor parameters 

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
                        

