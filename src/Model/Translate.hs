

import Generator.Gen
import Model.WSDL2Model 	

s = "S"


-- params <-> datatype
-- parameters <-> c/u de los valores del datatype
-- function <-> deffun

toFunction :: Function -> DefFun
toFunction (Function functionName params returnType _) =  DefFun name (map buildParams params) buildBody
