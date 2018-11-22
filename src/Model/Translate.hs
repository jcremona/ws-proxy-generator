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

import Control.Exception
import Control.Monad        hiding (forM_)
import Control.Monad.Reader
import Data.Either

import Text.XML.WSDL.Types hiding (types)
-- params <-> datatype
-- parameters <-> c/u de los valores del datatype
-- function <-> deffun

parsedWsdl :: Either SomeException WSDL
parsedWsdl = parseLBS $ fromStrict $(embedFile "/home/jcremona/ws-proxy-generator/test/hello.wsdl")

model = case parsedWsdl of
           Right wsdl -> runReader build wsdl
           _ -> error "Error during parsing"


translate ws = map ((cc (fn $ namespace ws) (types ws)) . ports) (defServices ws)
               where fn Nothing = error "Undefined namespace"
                     fn (Just t) = t

mk = map (\ms -> map (\ m -> runGenerationModule m) ms) $ translate model

cc namespace params ps = (map (\port -> flip ((Module $ unpack $ bName . fst $ port) . vvv namespace (snd port) . protocolBinding . fst) (map params2DataType params) port)  ps)

vvv namespace address pb = map ((toFunction namespace address . pFunction)) pb

-- TODO ver que pasa con el port y con el binding, leer los styles

toFunction :: Network.URI.URI -> Network.URI.URI -> Function  -> DefFun
toFunction  namespace address (Function functionName params returnType _) =  DefFun fname [("dt", Single . UserDefined . typeName $ dt)] (Call "invokeWS" [StringValue $ show address, StringValue fname, StringValue $ show namespace, StringValue "", ListValue $ map (\(c,_) -> TupleValue(StringValue c, Call c [Free "dt"])) $ constructors dt])
                                                           where dt = params2DataType $ messageType params
                                                                 fname = unpack functionName

 -- FIXME el param dt no puede ser igual a ninguna funcion??

params2DataType :: Params -> DataType
params2DataType (Params name parameters) = DataType (unpack name) $ map parameter2Constructor parameters 

parameter2Constructor :: Parameter -> (String, Type)
parameter2Constructor (Parameter (Name nl ns pr) ty) = (unpack nl, wsType2Type ty)

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

--sayHello :: 
--sayHello dt = callWS uri name namespace [(p1, ent dt), (p2, str dt)] responseName

--hd' :: [Int] -> Int
--hd' = head 

--myf :: [a] -> a
--myf hd' = hd' hd'	
