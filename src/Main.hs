import System.Environment
import Model.Translate
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

translate :: MonadThrow m => String -> m HaskellCode       
translate wsdlFile = do wsdl <- parseWsdl wsdlFile
                        model <- imodel wsdl
                        buildFromModel model
                        
run :: String -> IO ()
run wsdlFile = either (putStrLn . show) writeCode (translate wsdlFile)
