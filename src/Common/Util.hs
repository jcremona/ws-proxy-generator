module Common.Util where
import Data.Char

--------------------------------------------
-- Funciones de utilidad
--------------------------------------------

lowerFirstChar :: String -> String
lowerFirstChar (a:as) = (toLower a):as

upperFirstChar :: String -> String
upperFirstChar (a:as) = (toUpper a):as

