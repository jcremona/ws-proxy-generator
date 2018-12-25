module Model.CodeWriting (CodeWriting(..), TranslateFromModel(..)) where
import Model.ProxyModel
import Control.Monad.Catch (MonadThrow)

class CodeWriting a where
    writeCode :: a -> IO ()


class TranslateFromModel t where
    buildFromModel :: WSAbstraction -> t 
