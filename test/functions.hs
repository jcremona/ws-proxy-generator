import Data.Text hiding (map, head)
import Data.XML.Types
import Model.ProxyModel
import Text.XML.WSDL.Types hiding (types)

p = Params {wrapperName = pack "SayHelloRequest2", parameters = [Parameter {parameterName = Name {nameLocalName = pack "firstNam", nameNamespace = Nothing, namePrefix = Nothing}, ttype = WSPrimitiveType {primitiveType = WSInt}}]}
f = Function {functionName = pack "sayHello", params = NamedMsgs {messageName = Just $ pack "c", messageType = Params {wrapperName = pack "SayHelloRequest2", parameters = [Parameter {parameterName = Name {nameLocalName = pack "firstNam", nameNamespace = Nothing, namePrefix = Nothing}, ttype = WSPrimitiveType {primitiveType = WSInt}}]}}, returnType = NamedMsgs {messageName = Just $ pack "d", messageType = Params {wrapperName = pack "SayHelloResponse", parameters = [Parameter {parameterName = Name {nameLocalName = pack "greeting", nameNamespace = Nothing, namePrefix = Nothing}, ttype = WSPrimitiveType {primitiveType = WSString}}]}}, soapAction = ""}


op = ConcreteOperation {cOperationName = pack "sayHello", cOperationInput = Just (ConcreteInputMessage {cInputMessageName = Nothing, additionalConcreteInputInfo = [NodeElement (Element {elementName = Name {nameLocalName = pack "body", nameNamespace = Just $ pack  "http://schemas.xmlsoap.org/wsdl/soap/", namePrefix = Just $ pack  "soap"}, elementAttributes = [(Name {nameLocalName = pack "namespace", nameNamespace = Nothing, namePrefix = Nothing},[ContentText $ pack "http://examples.com/"]),(Name {nameLocalName = pack "use", nameNamespace = Nothing, namePrefix = Nothing},[ContentText $ pack "literal"])], elementNodes = []})]}), cOperationOutput = Just (ConcreteOutputMessage {cOutputMessageName = Nothing, additionalConcreteOutputInfo = [NodeElement (Element {elementName = Name {nameLocalName = pack "body", nameNamespace = Just $ pack  "http://schemas.xmlsoap.org/wsdl/soap/", namePrefix = Just $ pack  "soap"}, elementAttributes = [(Name {nameLocalName = pack "namespace", nameNamespace = Nothing, namePrefix = Nothing},[ContentText $ pack "http://examples.com/"]),(Name {nameLocalName = pack "use", nameNamespace = Nothing, namePrefix = Nothing},[ContentText $ pack "literal"])], elementNodes = []})]}), cOperationFault = [], additionalOperationInfo = [NodeElement (Element {elementName = Name {nameLocalName = pack "operation", nameNamespace = Just $ pack "http://schemas.xmlsoap.org/wsdl/soap/", namePrefix = Just $ pack  "soap"}, elementAttributes = [(Name {nameLocalName =pack  "soapAction", nameNamespace = Nothing, namePrefix = Nothing},[])], elementNodes = []})]}

fun = Function {functionName = pack "sayHello", params = NamedMsgs {messageName = Just $ pack  "sayHelloRequest", messageType = Params {wrapperName = pack "sayHello", parameters = [Parameter {parameterName = Name {nameLocalName = pack "firstNam", nameNamespace = Nothing, namePrefix = Nothing}, ttype = WSPrimitiveType {primitiveType = WSInt}}]}}, returnType = NamedMsgs {messageName = Just $ pack  "sayHelloResponse", messageType = Params {wrapperName = pack "sayHelloResponse", parameters = [Parameter {parameterName = Name {nameLocalName = pack "SayHelloResponse", nameNamespace = Nothing, namePrefix = Nothing}, ttype = WSPrimitiveType {primitiveType = WSString}}]}}, soapAction = ""}

data L = TInt | TList (TT L)
data TT a = Empty | T2 a deriving (Show, Eq)


a :: TT String
a = Empty

b :: TT Int
b = Empty

c = TList (T2 TInt)
