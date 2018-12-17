{-# LANGUAGE TemplateHaskell #-}
module Model.Translate (wsdlToModules) where

import Generator.Gen
import Model.WSDL2Model
import Data.Text hiding (map, head)
import Data.XML.Types
import qualified Data.Map as Map
import Model.ProxyModel
import Data.FileEmbed
import Text.XML.WSDL.Parser
import Data.ByteString.Lazy (fromStrict)
import Network.URI
import           Control.Monad.Catch          (MonadThrow)
import Control.Exception
import Common.Exception
import Control.Monad        hiding (forM_)
import Control.Monad.Reader
import Data.Either
import Text.XML.WSDL.Types hiding (types)
import qualified Data.ByteString as B
import Data.Text.Encoding (encodeUtf8)


portsToModules :: URI -> [Params] -> [(Port, URI)] -> Either SomeException [Module]
portsToModules namespace params = mapM (\(port, uri) -> runGenerationModule $ Module (unpack $ bName port) (bindingsToFunDefinitions namespace uri $ protocolBinding port) datatypes)
                                 where datatypes = map params2DataType params

bindingsToFunDefinitions :: URI -> URI -> [ProtocolBinding] -> [DefFun]
bindingsToFunDefinitions namespace address = map (toFunctionDefinition namespace address . pFunction)

toFunctionDefinition :: Network.URI.URI -> Network.URI.URI -> Function -> DefFun
toFunctionDefinition  namespace address (Function functionName params returnType _) =  DefFun (lowerFirstChar fname) [("dt", inputType)] 
                                                                     (Call "invokeWS" [StringValue $ show address, 
                                                                                       StringValue fname, 
                                                                                       StringValue "", 
                                                                                       StringValue $ show namespace, 
                                                                                       ListValue $ map (\(c,t) -> TupleValue(StringValue c, convertToString t $ Call c [Free "dt"])) $ constructors dt, 
                                                                                       StringValue return, 
                                                                                       ListValue responseParameterNames]) (Rec inputType $ IOMonad $ TList $ T $ Single $ TString)
                                                           where dt = params2DataType $ messageType params
                                                                 fname = unpack functionName
                                                                 inputType = Single . UserDefined . typeName $ dt
                                                                 return = unpack . wrapperName . messageType $ returnType
                                                                 responseParameterNames = map (StringValue . unpack . nameLocalName . parameterName) (parameters . messageType $ returnType)


 -- FIXME el param dt no puede ser igual a ninguna funcion??

params2DataType :: Params -> DataType
params2DataType (Params name parameters) = DataType (upperFirstChar . unpack $ name) $ map parameter2Constructor parameters 

parameter2Constructor :: Parameter -> (String, Type)
parameter2Constructor (Parameter (Name nl ns pr) ty) = (lowerFirstChar . unpack $ nl, wsType2Type ty)

wsType2Type (WSPrimitiveType WSString) = Single TString
wsType2Type (WSPrimitiveType WSInt) = Single TInt
wsType2Type (WSPrimitiveType WSDouble) = Single TDouble
wsType2Type (WSPrimitiveType WSLong) = Single TLong
wsType2Type (WSPrimitiveType WSChar) = Single TChar
wsType2Type (WSPrimitiveType WSFloat) = Single TFloat
wsType2Type (WSPrimitiveType WSVoid) = Single TVoid
wsType2Type (WSPrimitiveType WSBoolean) = Single TBool  

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

writeModuleList ms = mapM (\module_ -> write module_ >> putStrLn ("Module Generated: " ++ (moduleName module_))) ms
--             
writeModuleLists mss = do mapM writeModuleList mss
                          return ()

wsdlToModules path = do wsdlFile <- readFile path
                        either (putStrLn . show) writeModuleLists (translate wsdlFile) 
