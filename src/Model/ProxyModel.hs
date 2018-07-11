module Model.ProxyModel where

import Data.Text      (Text)
import Data.XML.Types

data LanguageAbstraction = LanguageAbstraction 
                         { interfaces       :: [Interface]
                         , enumerations     :: [Enumeration]
                         , lists            :: [List] -- Arrays (maxOccurs=unbounded)
                         , dataTypes        :: [DataType]
                         , protocolMetadata :: Metadata
                         }

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
               , messageType :: Params
               } deriving Show

data ProtocolBinding = ProtocolBinding
                     { bindingName :: Text
                     , pFunction :: Function
                     } deriving Show

data Params = Params
            { wrapperName :: Text
            , parameters :: [Parameter]
            } deriving Show

data Parameter = Parameter
               { parameterName :: Name
               , ttype         :: WSType 
               } deriving Show

data Enumeration = Enumeration 
                 { name    :: String
                 , members :: [EnumMembers]
                 }

data Port = Port
          { bName :: Text
          , protocolBinding :: [ProtocolBinding]
          } deriving Show

data EnumMembers = EnumMembers -- FIXME no me queda claro que tiene que contener EnumMembers

data Metadata = Metadata

data List = List -- FIXME ver como representar la jerarquia HWSType

data DataType = DataType

data FunctionIdentifier = FunctionIdentifier
                        { funName :: Text
                        , inputName :: Maybe Text
                        , outputName :: Maybe Text
                        }

data WSType = WSPrimitiveType 
            { primitiveType  :: PrimitiveType }
            | WSDataType
            { dataTypeName 	 :: String
            , hasParameters  :: Bool }
            | WSEnumType
            { enumName       :: String }
            | WSListType
            { listTypeName   :: String
            , listParameters :: [Parameter] }
            | WSPreType
            { preTypeName    :: String } deriving Show

data PrimitiveType = WSInt | WSDouble | WSLong | WSString | WSChar | WSFloat | WSVoid | WSBoolean deriving (Enum, Show)

