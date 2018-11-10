import Generator.Gen
import Model.WSDL2Model
import Data.Text hiding (map)
import Data.XML.Types
import qualified Data.Map as Map
import Model.ProxyModel

s = "S"


-- params <-> datatype
-- parameters <-> c/u de los valores del datatype
-- function <-> deffun

toFunction :: Function -> DefFun
toFunction (Function functionName params returnType _) =  DefFun (unpack functionName) [] (Call "callWS" [])

NamedMsgs 

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
 


--sayHello :: 
--sayHello dt = callWS uri name namespace [(p1, ent dt), (p2, str dt)] responseName
