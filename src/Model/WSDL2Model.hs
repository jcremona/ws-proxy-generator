module Model.WSDL2Model (buildModel) where

import Model.ProxyModel
import Control.Monad.Reader
import Data.Text      (Text, unpack, pack)
import Data.List
import Data.Maybe
import Data.XML.Types
import Text.XML.WSDL.Types
import Control.Monad.Catch (MonadThrow, throwM)
import Common.Exception

--type Error = String


--data Style = RPCLiteral | RPCEncoded | DocumentLiteral | DocumentLiteralWrapped

findM  :: (MonadThrow m) => (a -> Bool) -> [a] -> String -> m a
findM f ps e = case find f ps of
                 Nothing -> throwM $ throwCustomException e
                 Just p -> return p 

voidType :: NamedMsgs
voidType = NamedMsgs Nothing $ Params (pack "") [Parameter (Name (pack "") Nothing Nothing) (WSPrimitiveType WSVoid)]
       
paramsSearch :: Text -> Params -> Bool
paramsSearch t p = wrapperName p == t

getParams :: (MonadThrow m) => Reader WSDL (m [Params])
getParams = do ms <- asks messages
               return $ mapM buildParams ms

buildParams msg = do parameters <- mapM buildParameter $ wsdlMessageParts msg
                     return $ (Params (wsdlMessageName msg)) parameters

buildParameter part = do typ <- convertPrimitiveType $ unpack $ nameLocalName qname 
                         return $ Parameter (wsdlMessagePartName part) (WSPrimitiveType typ)
                  where qname = fromJust . wsdlMessagePartType $ part

buildFunction ps (AbstractOneWayOperation name input order) = do inp <- (findInputParam ps input)
                                                                 return $ Function name inp voidType ""
buildFunction ps (AbstractRequestResponseOperation name input output faults order) = do inp <- findInputParam ps input
                                                                                        out <- findOutputParam ps output
                                                                                        return $ Function name inp out ""
buildFunction ps (AbstractSolicitResponseOperation name output input faults order) = do inp <- findInputParam ps input
                                                                                        out <- findOutputParam ps output
                                                                                        return $ Function name inp out "" --FIXME
buildFunction ps (AbstractNotificationOperation name output order) = do out <- findOutputParam ps output 
                                                                        return $ Function name voidType out ""
                     

getInterfaces :: (MonadThrow m) => Reader WSDL (m [Interface])
getInterfaces = do pts <- asks portTypes
                   ms <- getParams
                   return $ do ms' <- ms
                               mapM (buildInterface ms') pts

buildInterface params portType = do fs <- (mapM (buildFunction params) (wsdlPortTypeOperations portType))
                                    return $ Interface (wsdlPortTypeName portType) fs

getPort :: (MonadThrow m) => Reader WSDL (m [Port])
getPort = do bs <- asks bindings
             is <- getInterfaces
             return $ do is' <- is
                         mapM (buildPort is') bs

buildPort is b = do i <- findInterface is b
                    protBindings <- mapM (buildProtocolBinding $ functions $ i) (wsdlBindingOperations b)
                    return $ Port (wsdlBindingName b) protBindings

buildProtocolBinding is op = do function <- (findFunc is op)
                                return $ ProtocolBinding (cOperationName op) function

getServices :: (MonadThrow m) => Reader WSDL (m [Service])
getServices = do svs <- asks services
                 bs <- getPort
                 return $ do bs' <- bs
                             mapM (buildService bs') svs

buildService ps ws = do ps' <- mapM (findPort ps) (wsdlServicePorts ws)
                        return $ Service (wsdlServiceName ws) ps' 


findInputParam ps input = do p <- findM (paramsSearch $ nameLocalName $ inputMessageType input) ps "Error looking for input parameters"
                             return $ NamedMsgs (inputMessageName input) p

findOutputParam ps output = do p <- findM (paramsSearch $ nameLocalName $ outputMessageType output) ps "Error looking for output parameters"
                               return $ NamedMsgs (outputMessageName output) p

findFunc fs op = findM (funcSearch op) fs "Error looking for function"

findInterface is b =  findM (ifaceSearch b) is "Error looking for portType"

findPort ps wp = do p <- findM (ptSearch wp) ps "Error looking for port"
                    case address wp of
                        Nothing -> throwM $ throwCustomException "Undefined address" 
                        Just addr -> return (p, addr)   

funcSearch :: ConcreteOperation -> Function -> Bool
funcSearch operation f = cOperationName operation == (functionName f) && concreteInputName operation == (messageName . params $ f) && concreteOutputName operation == (messageName . returnType $ f) 

ifaceSearch :: WSDLBinding -> Interface -> Bool
ifaceSearch b is = (nameLocalName . wsdlBindingType $ b) == interfaceName is 

ptSearch :: WSDLPort -> Port -> Bool
ptSearch wp p = (nameLocalName . wsdlPortBinding $ wp) == bName p
  
concreteOp :: ConcreteOperation -> FunctionIdentifier
concreteOp operation = FunctionIdentifier (cOperationName operation) (concreteInputName operation) (concreteOutputName operation)

concreteInputName :: ConcreteOperation -> Maybe Text
concreteInputName = (>>= cInputMessageName) . cOperationInput

concreteOutputName :: ConcreteOperation -> Maybe Text
concreteOutputName = (>>= cOutputMessageName) . cOperationOutput

convertPrimitiveType :: (MonadThrow m) => String -> m PrimitiveType
convertPrimitiveType "int" = return WSInt
convertPrimitiveType "double" = return WSDouble
convertPrimitiveType "long" = return WSLong
convertPrimitiveType "string" = return WSString
convertPrimitiveType "char" = return WSChar
convertPrimitiveType "float" = return WSFloat
convertPrimitiveType "void" = return WSVoid
convertPrimitiveType "boolean" = return WSBoolean
convertPrimitiveType st = throwM . throwCustomException $ "Unknown primitive type: " ++ st

buildModel :: (MonadThrow m) => Reader WSDL (m WSAbstraction)
buildModel = do servs <- getServices
                ts <- getParams
                ns <- asks targetNamespace
                return $ do servs' <- servs
                            ts' <- ts
                            return $ WSAbstraction servs' ts' ns

