{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE OverloadedStrings         #-}
module Callws where
import Network.HTTP
import Network.Browser
import Network.URI (parseURI)
import Data.Maybe (fromMaybe)
import Data.Char (toUpper, toLower)
import Data.Text.Encoding (encodeUtf8)
import Data.ByteString.Lazy (fromStrict)
import Control.Exception (SomeException, displayException)
import           Control.Monad.Reader
import           Data.Text                    (Text)
import qualified Data.Text                    as T
import           Text.XML.Stream.Parse 
import           Data.XML.Types
import           Data.Conduit
import           Control.Monad.Catch          (MonadThrow)
type Parameter = (String,String) -- name and value

data ParseState = ParseState
                { psDocumentNamespace :: Maybe Text
                -- , psDocumentSchemas   :: [Schema]
                } deriving Show

emptyParseState :: ParseState
emptyParseState = ParseState Nothing -- []

class XmlSerializable t where
     toXml :: t -> String
     fromXml :: String -> t
     
buildSOAPRequest uri action content = 
    Request { rqURI=uri
            , rqBody=content
            , rqHeaders=[ Header HdrContentType "text/xml; charset=utf-8"
                        , Header HdrContentLength (show (length content))
                        , Header HdrUserAgent libUA
                        , Header (HdrCustom "HdrSoapAction") action
                        ]
            , rqMethod=POST
            }

xmlTagStart :: String -> String
xmlTagStart s = '<':s ++ ['>']

xmlTagEnd :: String -> String
xmlTagEnd s = '<':'/':s ++ ['>']

encode = fromStrict . encodeUtf8 . T.pack

buildXml :: String -> String -> String
buildXml elemName elemValue = (xmlTagStart elemName) ++ elemValue ++ (xmlTagEnd elemName)

buildPrimitiveXmlList :: Show t => [t] -> String -> String
buildPrimitiveXmlList elems elemsName =	foldr (++) [] (map ((buildXml elemsName).primitiveToStr) elems)

buildComplexXmlList :: XmlSerializable t => [t] -> String -> String
buildComplexXmlList elems elemsName = foldr (++) [] (map ((buildXml elemsName).toXml) elems)

primitiveToStr :: Show t => t -> String
primitiveToStr t
	| head str == '"' || head str == '\'' = (tail (take (length str - 1) str))
	| otherwise = str
	where str = show t

soapXmlHeader :: String
soapXmlHeader = "<?xml version='1.0' encoding='utf-8'?>" ++
                "<soap:Envelope xmlns:xsi='http://www.w3.org/2001/XMLSchema-instance' xmlns:xsd='http://www.w3.org/2001/XMLSchema' xmlns:soap='http://schemas.xmlsoap.org/soap/envelope/'> " ++
                "<soap:Body>"

soapXmlFooter :: String
soapXmlFooter = "</soap:Body>" ++
		        "</soap:Envelope>"

nsalias = "nsalias"

functionToXml :: String -> String -> [Parameter] -> String
functionToXml name namespace parameters = 
	"<" ++ nsalias ++ ":" ++ name ++ " xmlns:" ++ nsalias ++ "=\'" ++ namespace ++ "\'>" ++
	parametersToXml parameters ++
	"</" ++ nsalias ++ ":" ++ name ++ ">"

parametersToXml :: [Parameter] -> String
parametersToXml [] = []
parametersToXml ((name,value):ps) = 
	"<" ++ name ++ ">" ++  value ++ "</" ++ name ++ ">" ++
	parametersToXml ps

invokeWS :: String -> String -> String -> [Parameter] -> String -> [String] -> IO [String]
invokeWS uriStr methodName namespace parameters response responseTags = do
	xmlResp <- browse $ fn methodName namespace parameters
	--putStrLn xmlResp
	--putStrLn $ "in: " ++ contentTst
	--putStrLn $ "out: " ++ xmlResp
	return $ displayResponse xmlResp response responseTags --head $ (getNodeValues xmlResp response)
	where uri = fromMaybe (error "Nothing from url parse") (parseURI uriStr)
	      contentTst = soapXmlHeader
	                   ++ (functionToXml methodName namespace parameters)
	                   ++ soapXmlFooter
	      req = buildSOAPRequest uri "" contentTst
	      fn methodName namespace parameters = do
			setCookieFilter (\_ _ -> return True)
			setErrHandler (\_ -> return ())
			setOutHandler (\_ -> return ())
			rsp <- request req
			return (rspBody $ snd rsp)

-- get the values of xml nodes specified by their name
-- ex.: getNodeValues "<a>1</a><b>2</b><a>3</a>" a = [1,3]
-- ex.: getNodeValues "<a>1</a><b>2</b><a>3</a>" b = [2]
getNodeValues :: String -> String -> [String]
getNodeValues [] nodeName = []
getNodeValues xml nodeName
	| (take (length xmlElement) xml) == xmlElement
		= let
			(nodeValue, restXml) = buildNodeValue (drop (length xmlElement) xml) nodeName []
          in
             nodeValue:(getNodeValues restXml nodeName)
	| otherwise = getNodeValues (tail xml) nodeName
	where xmlElement = xmlTagStart nodeName

-- returns the value of the xml element and the rest of the xml being parsed
buildNodeValue :: String -> String -> String -> (String, String)
buildNodeValue restXml nodeName accum
	| (take (length xmlElement) restXml) == xmlElement = (reverse accum, drop (length xmlElement) restXml)
	| otherwise = buildNodeValue (tail restXml) nodeName ((head restXml):accum)
	where xmlElement = xmlTagEnd nodeName

--------------------------------------------------------------------------------------
parseXmlResponse :: String -> String -> [String] -> Either SomeException [String]
parseXmlResponse = parseResponse . encode 

displayResponse xml responseTag elementTags  = either ((:[]) . displayException) id (parseXmlResponse xml responseTag elementTags)

--inputXml = "<?xml version=\"1.0\" ?><S:Envelope xmlns:S=\"http://schemas.xmlsoap.org/soap/envelope/\"><S:Body><ns2:sayHelloResponse xmlns:ns2=\"http://examples.com/\"><return>nuevo metodo, response: s</return></ns2:sayHelloResponse></S:Body></S:Envelope>"


--inputXml' = "<?xml version=\"1.0\" encoding=\"UTF-8\"?><soap:Envelope xmlns:nsalias=\"http://example.com/pysimplesoapsamle/\" xmlns:soap=\"http://schemas.xmlsoap.org/soap/envelope/\" xmlns:xsd=\"http://www.w3.org/2001/XMLSchema\" xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\"><soap:Body><EchoResponse xmlns=\"nsalias\"><output0>hello</output0><output1>4</output1></EchoResponse></soap:Body></soap:Envelope>"

--parseEnvelope :: MonadThrow m => [String] -> ConduitM Event o (ReaderT ParseState m) [String]
parseResponse t rsp bs = runReaderT (parseLBS def t $$ (parseEnvelope rsp bs)) emptyParseState

--parseEnvelope :: MonadThrow m => String -> [String] -> ConduitM Event o (ReaderT ParseState m) (Maybe (Maybe (Maybe [Maybe String])))
parseEnvelope rsp bts = force "Missing Envelope" $ tag (matching $ (== "Envelope") . nameLocalName)
                    (\ n -> return n)
                    (\ n -> local (\ a -> a { psDocumentNamespace = nameNamespace n }) $ do
                       ts <- force "Missing Body" $ parseBody rsp bts
                       return ts)


--parseBody :: MonadThrow m => [String] -> ConduitM Event o (ReaderT ParseState m) [String]
parseBody rsp bodyTags = tagNS "Body" ignoreAttrs 
                   (\_ -> force "Missing Body Element" $ parseBodyResponse rsp bodyTags)

parseBodyResponse rsp bodyTags = tag (matching $ (\n -> nameLocalName n == (T.pack $ rsp))) (const ignoreAttrs) (\_ -> parseBodyElements bodyTags) 
 
-- CONSIDERAR HACERLO CON many
--parseBodyElements :: MonadThrow m => [String] -> ConduitM Event o (ReaderT ParseState m) [String]
parseBodyElements bodyTags = mapM (\bTag -> force ("Missing tag: " ++ bTag) $ tag (matching $ (\n -> nameLocalName n == (T.pack $ bTag))) (const ignoreAttrs) (\v -> do txt <- content
                                                                                                                                                                        return $ T.unpack txt)) bodyTags

tagNS :: (MonadReader ParseState m, MonadThrow m)
      => Text
      -> AttrParser a
      -> (a -> ConduitM Event o m b)
      -> ConduitM Event o m (Maybe b)
tagNS t a p = do
    ns <- asks psDocumentNamespace
    tag (matching (\ n -> nameLocalName n == t && nameNamespace n == ns)) (const a) p

-------------------------------------------------------


-------------------------------------
-- Helper manipulation functions
-------------------------------------

showInt :: Int -> String
showInt = show

showDouble :: Double -> String
showDouble = show

showLong :: Integer -> String
showLong = show

showChar :: Char -> String
showChar = show

showFloat :: Float -> String
showFloat = show

showBool :: Bool -> String
showBool = show

showVoid :: () -> String
showVoid = show

readInt :: String -> Int
readInt = read

readDouble :: String -> Double
readDouble = read

readLong :: String -> Integer
readLong = read

readChar :: String -> Char
readChar = read

readFloat :: String -> Float
readFloat = read

readBool :: String -> Bool
readBool = read

readVoid :: String -> ()
readVoid = read

takeString :: [String] -> Int -> String
takeString = (!!)

lowerFirstChar :: String -> String
lowerFirstChar (a:as) = (toLower a):as

upperFirstChar :: String -> String
upperFirstChar (a:as) = (toUpper a):as

headStr :: [String] -> String
headStr [] = ""
headStr (a:as) = a
