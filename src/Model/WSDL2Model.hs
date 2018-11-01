module Model.WSDL2Model where

import Model.ProxyModel
import Control.Monad.Reader
import Data.Text      (Text, unpack, pack)
import Data.List
import Data.Maybe
import Data.XML.Types
import Text.XML.WSDL.Types
import Control.Monad.Catch (MonadThrow, throwM)
type Error = String
-- TODO ver en AssignParameters para que sirven los pretypes (para cuando no se encuentra un tipo en un paso previo)
-- TODO ver MountFunctions, ya que tomaremos solo tipos basicos tomados direct de los parts de los Messages

--data QName = QName
--	{ localName :: Text
--	, namespace :: Text
--	}
--	deriving (Show)

--instance Eq QName where
--	(==) = (==) `on` (\x -> (namespace x, localName x))

-- usar lookup
--type Environment = [(QName,DataType)]
--wsdl2Model :: WSDL -> LanguageAbstraction
--wsdl2Model wsdl = null



--operations :: (QName -> Parameter) -> Reader WSDL Function
--operations f = do msgs <- asks messages
                  
                  

--binds :: Reader WSDL Function
--binds = do b <- asks bindings
           

-- Agregar un Maybe Text junto con el Params devuelto
findInputParam :: [Params] -> InputMessage -> NamedMsgs
findInputParam ps input = case find (paramsSearch $ (nameLocalName . inputMessageType) input) ps of
                              Nothing -> error "input"
                              Just p -> NamedMsgs (inputMessageName input) p

findOutputParam :: [Params] -> OutputMessage -> NamedMsgs
findOutputParam ps output = case find (paramsSearch $ (nameLocalName . outputMessageType) output) ps of
                              Nothing -> error "output"
                              Just p -> NamedMsgs (outputMessageName output) p

voidType :: NamedMsgs
voidType = NamedMsgs Nothing $ Params (pack "") [Parameter (Name (pack "") Nothing Nothing) (WSPrimitiveType WSVoid)]

absOps :: [Params] -> AbstractOperation -> Function
absOps ps (AbstractOneWayOperation name input order) = Function name (findInputParam ps input) voidType ""
absOps ps (AbstractRequestResponseOperation name input output faults order) = Function name (findInputParam ps input) (findOutputParam ps output) ""
absOps ps (AbstractSolicitResponseOperation name output input faults order) = Function name (findInputParam ps input) (findOutputParam ps output) "" --FIXME
absOps ps (AbstractNotificationOperation name output order) = Function name voidType (findOutputParam ps output) ""
                                                          

paramsSearch :: Text -> Params -> Bool
paramsSearch t p = wrapperName p == t


interface_ :: Reader WSDL [Interface]
interface_ = do pts <- asks portTypes
                ms <- msgs
                mapM (oper ms) pts

oper :: [Params] -> WSDLPortType -> Reader WSDL Interface
oper params portType = return $ Interface (wsdlPortTypeName portType) (map (absOps params) (wsdlPortTypeOperations portType))

binding :: Reader WSDL [Port]
binding = do bs <- asks bindings
             is <- interface_
             mapM (bb is) bs

concOps :: [Function] -> ConcreteOperation -> ProtocolBinding
concOps is op = ProtocolBinding (cOperationName op) (findFunc is op)

findFunc :: [Function] -> ConcreteOperation -> Function
findFunc fs op = case find (funcSearch op) fs of
                    Nothing -> error "funcs"
                    Just f -> f


-- TODO arrays! unbounded

--type de wsdl binding
-- interface <---> wsdlbinding
bb :: [Interface] -> WSDLBinding -> Reader WSDL Port
bb is b = return $ Port (wsdlBindingName b) (map (concOps $ functions $ findInterface is b) (wsdlBindingOperations b))
        

findInterface :: [Interface] -> WSDLBinding -> Interface
findInterface is b =  case find (ifaceSearch b) is of
                Nothing -> error "interf"
                Just i -> i
------------------ TODO
ifaceSearch :: WSDLBinding -> Interface -> Bool
ifaceSearch b is = (nameLocalName . wsdlBindingType $ b) == interfaceName is -- FIXME

pp :: [Port] -> WSDLService -> Reader WSDL Service
pp ps ws = return $ Service (wsdlServiceName ws) (map (findPort ps) (wsdlServicePorts ws)) 

ss :: Reader WSDL [Service]
ss = do svs <- asks services
        bs <- binding
        mapM (pp bs) svs

ptSearch :: WSDLPort -> Port -> Bool
ptSearch wp p = (nameLocalName . wsdlPortBinding $ wp) == bName p -- FIXME

findPort :: [Port] -> WSDLPort -> Port
findPort ps wp = case find (ptSearch wp) ps of
                    Nothing -> error "ports"
                    Just p -> p   

funcSearch :: ConcreteOperation -> Function -> Bool
funcSearch operation f = cOperationName operation == (functionName f) && concreteInputName operation == (messageName . params $ f) && concreteOutputName operation == (messageName . returnType $ f) 

concreteOp :: ConcreteOperation -> FunctionIdentifier
concreteOp operation = FunctionIdentifier (cOperationName operation) (concreteInputName operation) (concreteOutputName operation)

concreteInputName :: ConcreteOperation -> Maybe Text
concreteInputName = (>>= cInputMessageName) . cOperationInput
                              --do inp <- cOperationInput operation
                              --   cInputMessageName inp

concreteOutputName :: ConcreteOperation -> Maybe Text
concreteOutputName = (>>= cOutputMessageName) . cOperationOutput

parts :: WSDLMessagePart -> Parameter
parts part = Parameter (wsdlMessagePartName part) (WSPrimitiveType $ convertPrimitiveType $ unpack $ nameLocalName qname)
                  where qname = fromJust . wsdlMessagePartType $ part

msgs :: Reader WSDL [Params]
msgs = do ms <- asks messages
          mapM ftt ms

ftt :: WSDLMessage -> Reader WSDL Params
ftt msg = return $ Params (wsdlMessageName msg) (map parts $ wsdlMessageParts msg)

build :: Reader WSDL WSAbstraction
build = do servs <- ss
           ts <- msgs
           return $ WSAbstraction servs ts

--recorrer el binding, en el entorno deberían estar las funciones. el entorno debería ser un mapa id/name -> funcion, algo parecido para los parametros. 
--el problema es como meter los valores en el entorno. además serían entornos distinto tipo para las func y los parametros, ver como abstraerlo. la idea es no pasar el entorno explicitamente.
--otro problema es como secuencialmente construir primero los parametros, luego las func y luego el binding.


convertPrimitiveType :: String -> PrimitiveType
convertPrimitiveType "int" = WSInt
convertPrimitiveType "double" = WSDouble
convertPrimitiveType "long" = WSLong
convertPrimitiveType "string" = WSString
convertPrimitiveType "char" = WSChar
convertPrimitiveType "float" = WSFloat
convertPrimitiveType "void" = WSVoid
convertPrimitiveType "boolean" = WSBoolean
convertPrimitiveType st = error $ "prim type unknown " ++ st
