{-# LANGUAGE TemplateHaskell #-}
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
import Control.Monad        hiding (forM_)
import Control.Monad.Reader
import Data.Either

import Text.XML.WSDL.Types hiding (types)

parsedWsdl :: (MonadThrow m) => m WSDL
parsedWsdl = parseLBS $ fromStrict $(embedFile "/home/jcremona/ws-proxy-generator/test/hello_world.wsdl")


imodel :: (MonadThrow m) => m WSAbstraction
imodel = parsedWsdl >>= (return . runReader build)

maybeToEither = flip maybe Right . Left

translate' :: Either SomeException [[Module]]
translate' = do model <- imodel
                nspace <- maybeToEither (throwCustomException "Undefined Namespace") $ namespace model
                mapM ((portsToModules nspace $ types model) . ports) (defServices model)

writeModuleList ms = mapM (\module_ -> write module_ >> putStrLn ("Module Generated: " ++ (moduleName module_))) ms
             
writeModuleLists mss = do mapM writeModuleList mss
                          return ()

main = either (putStrLn . show) writeModuleLists translate' 

portsToModules :: URI -> [Params] -> [(Port, URI)] -> Either SomeException [Module]
portsToModules namespace params = mapM (\(port, uri) -> runGenerationModule $ Module (unpack $ bName port) (bindingsToFunDefinitions namespace uri $ protocolBinding port) datatypes)
                                 where datatypes = map params2DataType params

bindingsToFunDefinitions :: URI -> URI -> [ProtocolBinding] -> [DefFun]
bindingsToFunDefinitions namespace address = map (toFunctionDefinition namespace address . pFunction)

toFunctionDefinition :: Network.URI.URI -> Network.URI.URI -> Function -> DefFun
toFunctionDefinition  namespace address (Function functionName params returnType _) =  DefFun (lowerFirstChar fname) [("dt", Single . UserDefined . typeName $ dt)] 
                                                                     (Call "invokeWS" [StringValue $ show address, 
                                                                                       StringValue fname, 
                                                                                       StringValue "", 
                                                                                       StringValue $ show namespace, 
                                                                                       ListValue $ map (\(c,t) -> TupleValue(StringValue c, convertToString t $ Call c [Free "dt"])) $ constructors dt, 
                                                                                       StringValue return, 
                                                                                       ListValue responseParameterNames])
                                                           where dt = params2DataType $ messageType params
                                                                 fname = unpack functionName
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
 


p = Params {wrapperName = pack "SayHelloRequest2", parameters = [Parameter {parameterName = Name {nameLocalName = pack "firstNam", nameNamespace = Nothing, namePrefix = Nothing}, ttype = WSPrimitiveType {primitiveType = WSInt}}]}
f = Function {functionName = pack "sayHello", params = NamedMsgs {messageName = Just $ pack "c", messageType = Params {wrapperName = pack "SayHelloRequest2", parameters = [Parameter {parameterName = Name {nameLocalName = pack "firstNam", nameNamespace = Nothing, namePrefix = Nothing}, ttype = WSPrimitiveType {primitiveType = WSInt}}]}}, returnType = NamedMsgs {messageName = Just $ pack "d", messageType = Params {wrapperName = pack "SayHelloResponse", parameters = [Parameter {parameterName = Name {nameLocalName = pack "greeting", nameNamespace = Nothing, namePrefix = Nothing}, ttype = WSPrimitiveType {primitiveType = WSString}}]}}, soapAction = ""}


op = ConcreteOperation {cOperationName = pack "sayHello", cOperationInput = Just (ConcreteInputMessage {cInputMessageName = Nothing, additionalConcreteInputInfo = [NodeElement (Element {elementName = Name {nameLocalName = pack "body", nameNamespace = Just $ pack  "http://schemas.xmlsoap.org/wsdl/soap/", namePrefix = Just $ pack  "soap"}, elementAttributes = [(Name {nameLocalName = pack "namespace", nameNamespace = Nothing, namePrefix = Nothing},[ContentText $ pack "http://examples.com/"]),(Name {nameLocalName = pack "use", nameNamespace = Nothing, namePrefix = Nothing},[ContentText $ pack "literal"])], elementNodes = []})]}), cOperationOutput = Just (ConcreteOutputMessage {cOutputMessageName = Nothing, additionalConcreteOutputInfo = [NodeElement (Element {elementName = Name {nameLocalName = pack "body", nameNamespace = Just $ pack  "http://schemas.xmlsoap.org/wsdl/soap/", namePrefix = Just $ pack  "soap"}, elementAttributes = [(Name {nameLocalName = pack "namespace", nameNamespace = Nothing, namePrefix = Nothing},[ContentText $ pack "http://examples.com/"]),(Name {nameLocalName = pack "use", nameNamespace = Nothing, namePrefix = Nothing},[ContentText $ pack "literal"])], elementNodes = []})]}), cOperationFault = [], additionalOperationInfo = [NodeElement (Element {elementName = Name {nameLocalName = pack "operation", nameNamespace = Just $ pack "http://schemas.xmlsoap.org/wsdl/soap/", namePrefix = Just $ pack  "soap"}, elementAttributes = [(Name {nameLocalName =pack  "soapAction", nameNamespace = Nothing, namePrefix = Nothing},[])], elementNodes = []})]}

fun = Function {functionName = pack "sayHello", params = NamedMsgs {messageName = Just $ pack  "sayHelloRequest", messageType = Params {wrapperName = pack "sayHello", parameters = [Parameter {parameterName = Name {nameLocalName = pack "firstNam", nameNamespace = Nothing, namePrefix = Nothing}, ttype = WSPrimitiveType {primitiveType = WSInt}}]}}, returnType = NamedMsgs {messageName = Just $ pack  "sayHelloResponse", messageType = Params {wrapperName = pack "sayHelloResponse", parameters = [Parameter {parameterName = Name {nameLocalName = pack "SayHelloResponse", nameNamespace = Nothing, namePrefix = Nothing}, ttype = WSPrimitiveType {primitiveType = WSString}}]}}, soapAction = ""}
--sayHello :: 
--sayHello dt = callWS uri name namespace [(p1, ent dt), (p2, str dt)] responseName

--hd' :: [Int] -> Int
--hd' = head 

--myf :: [a] -> a
--myf hd' = hd' hd'	
