module Common.Exception where
import Control.Exception

data CustomException = CustomException {
                            errorMessage :: String
                       } deriving (Show)

instance Exception CustomException where

throwCustomException = toException . CustomException
