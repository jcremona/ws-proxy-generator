module Model.CodeWriting (CodeWriting(..), TranslateFromModel(..)) where
import Model.ProxyModel

----------------------------------------------------------------------------------------------
-- Clases que fuerzan la implementación de dos funciones
-- para el lenguaje target para el que se quiera generar código.
----------------------------------------------------------------------------------------------

class CodeWriting a where
    writeCode :: a -> IO ()


class TranslateFromModel t where
    buildFromModel :: WSAbstraction -> t 
