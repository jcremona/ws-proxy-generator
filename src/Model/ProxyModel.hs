module Model.Proxy where


data LanguageAbstraction = LanguageAbstraction 
                         { functions    :: [Function]
                         , enumerations :: [Enumeration]
                         , lists        :: [List]
                         , dataTypes    :: [DataType]
                         }

data Function = Function
              { functionName :: String
              , returnType   :: String -- FIXME ver como representar la jerarquia HWSType
              , soapAction   :: String
              , parameters   :: [Parameter]
              }

data Parameter = Parameter
               { parameterName :: String
               , ttype         :: String -- FIXME ver como representar la jerarquia HWSType
               }

data Enumeration = Enumeration 
                 { name    :: String
                 , members :: [EnumMembers]
                 }

data EnumMembers = EnumMembers -- FIXME no me queda claro que tiene que contener EnumMembers

data List = List -- FIXME ver como representar la jerarquia HWSType

data DataType = DataType
