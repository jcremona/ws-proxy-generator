module Common.Exception where
import Control.Exception


-- Instance de Exception que se utilizará en los módulos superiores para el manejo de errores.
data CustomException = CustomException {
                            errorMessage :: String
                       } deriving (Show)

instance Exception CustomException where

throwCustomException = toException . CustomException
