import System.Environment
import Model.HaskellTranslate
import Model.CodeWriting
import qualified Data.ByteString as B
import Control.Monad.Catch (MonadThrow, SomeException)
import Text.XML.WSDL.Types
import Text.XML.WSDL.Parser
import Model.ProxyModel
import Model.WSDL2Model
import Data.Text hiding (map, head,zip)
import Data.ByteString.Lazy (fromStrict)
import Data.Text.Encoding (encodeUtf8)
import Control.Monad.Reader
import Network.HTTP

---------------------------------------------------
-- El punto de entrada de todo el proyecto.
-- Toma como argumento de entrada
-- la url del documento WSDL.
---------------------------------------------------
main = do args <- getArgs
          case args of
             [file] -> wsdlToCode file
             _ -> putStrLn "Error: unknown args"

wsdlToCode :: String -> IO ()
wsdlToCode path = do response <- simpleHTTP $ getRequest path
                     case (fmap rspBody response) of
                             Left err -> putStrLn . show $ err
                             Right wsdlFile -> run wsdlFile


packStr :: String -> B.ByteString
packStr = encodeUtf8 . pack

parseWsdl :: (MonadThrow m) => String -> m WSDL
parseWsdl = parseLBS . fromStrict . packStr

imodel :: (MonadThrow m) => WSDL -> m WSAbstraction
imodel = runReader buildModel 



------------------------------------------------
-- translate: aqui se decide el lenguaje target
-- que se utilizará en este proyecto. En este
-- caso HaskellCode es una representación 
-- de Haskell, para el cual se implementó
-- buildFromModel y writeCode 
-- (ambos en HaskellTranslate)  
------------------------------------------------
translate :: MonadThrow m => String -> m HaskellCode       
translate wsdlFile = do wsdl <- parseWsdl wsdlFile
                        model <- imodel wsdl
                        buildFromModel model
                        
------------------------------------------------
-- run: aqui se decide la instancia de MonadThrow
-- que se utilizará en todo el proyecto, 
-- en este caso es Either SomeException. 
------------------------------------------------
run :: String -> IO ()
run wsdlFile = either (putStrLn . show) writeCode (translate wsdlFile)
