module Model.ProxyModel where

import Data.Text      (Text)
import Data.XML.Types
import Network.URI

--------------------------------------------------------------
-- Abstracción simplificada de los elementos de WSDL
--------------------------------------------------------------


data WSAbstraction = WSAbstraction 
                   { defServices :: [Service]
                   , types    :: [Message]
                   , namespace :: Maybe URI
                   } deriving Show
                   


data Interface = Interface
               { interfaceName :: Text
               , functions     :: [Function]
               } deriving Show

data Function = Function
              { functionName :: Text
              , params       :: NamedMsgs
              , returnType   :: NamedMsgs 
              , soapAction   :: String
              } deriving Show

data NamedMsgs = NamedMsgs
               { messageName :: Maybe Text
               , messageType :: Message
               } deriving Show

data ProtocolBinding = ProtocolBinding
                     { bindingName :: Text
                     , pFunction :: Function
                     } deriving Show

data Message = Message
            { wrapperName :: Text
            , parameters :: [Parameter]
            } deriving Show

-- FIXME rename to Part
data Parameter = Parameter
               { parameterName :: Name
               , ttype         :: WSType 
               } deriving Show


data Port = Port
          { bName :: Text
          , protocolBinding :: [ProtocolBinding]
          } deriving Show

data Service = Service
             { sName :: Text
             , ports :: [(Port, URI)]
             } deriving Show

data FunctionIdentifier = FunctionIdentifier
                        { funName :: Text
                        , inputName :: Maybe Text
                        , outputName :: Maybe Text
                        }

data WSType = WSPrimitiveType 
            { primitiveType  :: PrimitiveType }
 --           | WSDataType
 --           { dataTypeName 	 :: String
 --           , hasParameters  :: Bool }
 --           | WSEnumType
 --           { enumName       :: String }
 --           | WSListType
 --           { listTypeName   :: String
 --           , listParameters :: [Parameter] }
 --           | WSPreType
 --           { preTypeName    :: String } 
          deriving Show

data PrimitiveType = WSInt | WSDouble | WSLong | WSString | WSChar | WSFloat | WSVoid | WSBoolean deriving (Enum, Show)

