{-# LANGUAGE CPP                       #-}
{-# LANGUAGE FlexibleContexts          #-}
--{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE RankNTypes                #-}
{-# LANGUAGE RecordWildCards           #-}

-- | For parsing WSDLs.
module Text.XML.WSDL.Parser (
  parseLBS--, parseFile
) where

#if __GLASGOW_HASKELL__ <= 708
import           Control.Applicative          (pure, (<$>))
#endif
import           Control.Applicative          ((<|>))
import           Control.Monad
import           Control.Monad.Catch          (MonadThrow, throwM)
import           Control.Monad.Reader
--import           Control.Monad.Trans.Resource (MonadResource)
import           Data.ByteString.Lazy         (ByteString)
import           Data.Conduit
import           Data.Maybe
import           Data.String
import           Data.Text                    (Text)
import qualified Data.Text                    as T
import           Data.XML.Types
import           Network.URI                  (parseURI)
import           Text.XML.Stream.Parse        hiding (parseFile, parseLBS)
import qualified Text.XML.Stream.Parse        as P

import           Text.XML.WSDL.Types

data ParseState = ParseState
                { psDocumentNamespace :: Maybe Text
                -- , psDocumentSchemas   :: [Schema]
                } deriving Show

type Parser a = forall m o. (MonadThrow m, MonadReader ParseState m) => ConduitM Event o m a

emptyParseState :: ParseState
emptyParseState = ParseState Nothing -- []

-- | Parse a 'ByteString' into a WSDL.
parseLBS :: MonadThrow m => ByteString -> m WSDL
parseLBS t = runReaderT (runConduit $ P.parseLBS def t .| parseWSDL) emptyParseState

-- | Parse a file into a WSDL.
--parseFile :: MonadResource m => FilePath -> m WSDL
--parseFile f = runReaderT (P.parseFile def f $$ parseWSDL) emptyParseState

ignoreDocs :: MonadThrow m => ConduitM Event Event m b
ignoreDocs = forever $ do
    p <- await
    case p of
        Just d@(EventBeginElement n _) | nameLocalName n == "documentation" -> do
            leftover d
            void $ ignoreTreeContent (matching $ (== "documentation") . nameLocalName)
        Just x -> yield x
        Nothing -> return ()

parseWSDL :: MonadThrow m => ConduitM Event o (ReaderT ParseState m) WSDL
parseWSDL = (ignoreDocs .|) $ force "Missing WSDL" $ tag
    (matching $ (== "definitions") . nameLocalName)
    (\ n -> do
        tns <- (>>= parseURI . T.unpack) <$> attr "targetNamespace"
        docname <- (textToName <$>) <$> attr "name"
        return (n, tns, docname)
        )
    (\ (n, tns, docname) -> local (\ a -> a { psDocumentNamespace = nameNamespace n }) $ do
        xel <- many parseXElement
        tys <- parseTypes
        messages <- many parseMessage
        portTypes <- many parsePortType
        bindings <- many parseBinding
        services <- many parseService
        return $ WSDL tns docname tys messages portTypes bindings services xel)

parseTypes :: Parser (Maybe WSDLTypes)
parseTypes = tagNS "types" ignoreAttrs
    (\ _ -> do
        ss <- many parseSchema
        return $ WSDLTypes ss [])

-- ignoreAllTreesContent -> ignoreAnyTreeContent
parseSchema :: MonadThrow m => ConduitM Event o m (Maybe Schema)
parseSchema = tag (matching $ (== "schema") . nameLocalName) (const ignoreAttrs)
    (\ _ -> many ignoreAnyTreeContent >> return Schema)

parseMessage :: Parser (Maybe WSDLMessage)
parseMessage = tagNS "message" (requireAttr "name")
    (\ t -> do
        parts <- many parsePart
        return $ WSDLMessage t parts)

parsePart :: Parser (Maybe WSDLMessagePart)
parsePart = tagNS "part"
    (liftM3 (,,)
        (requireAttr "name")
        (attr "element")
        (attr "type"))
    (\ (n,e,t) -> do
        unless (isJust e || isJust t) $
            throwM $ XmlException "Element or type required" Nothing
        return $ WSDLMessagePart
            (textToName n)
            (textToName <$> e)
            (textToName <$> t))

parsePortType :: Parser (Maybe WSDLPortType)
parsePortType = tagNS "portType" (requireAttr "name")
    (\ n -> do
        ops <- many parseAbstractOperation
        return $ WSDLPortType n ops)

parseAbstractOperation :: Parser (Maybe AbstractOperation)
parseAbstractOperation = choose [parseOpReqRes]

parseOpReqRes :: Parser (Maybe AbstractOperation)
parseOpReqRes = opTag $ \ (n, order) -> do
    inpM <- parseInputMessage
    case inpM of
        Nothing -> return Nothing
        Just inp -> do
            oupM <- parseOutputMessage
            case oupM of
                Nothing -> return Nothing
                Just oup -> do
                    faults <- many parseFault
                    return . Just $ AbstractRequestResponseOperation n inp oup faults (maybeToList order)

parseBinding :: Parser (Maybe WSDLBinding)
parseBinding = tagNS "binding"
    (liftM2 (,) (requireAttr "name") (requireAttr "type"))
    (\ (n,t) -> do
        xel <- many parseXElement
        ops <- many parseBindingOperation
        return $ WSDLBinding n (textToName t) ops xel)

parseBindingOperation :: Parser (Maybe ConcreteOperation)
parseBindingOperation = tagNS "operation"
    (requireAttr "name")
    (\ n -> do
        xel <- many parseXElement
        cin <- parseCInputMessage
        cout <- parseCOutputMessage
        faults <- many parseCFaultMessage
        return $ ConcreteOperation n cin cout faults xel)

parseCInputMessage :: Parser (Maybe ConcreteInputMessage)
parseCInputMessage = tagNS "input" (attr "name")
    (\ n -> ConcreteInputMessage n <$> many parseXElement)

parseCOutputMessage :: Parser (Maybe ConcreteOutputMessage)
parseCOutputMessage = tagNS "output" (attr "name")
    (\ n -> ConcreteOutputMessage n <$> many parseXElement)

parseCFaultMessage :: Parser (Maybe ConcreteFaultMessage)
parseCFaultMessage = tagNS "output" (requireAttr "name")
    (\ n -> ConcreteFaultMessage n <$> many parseXElement)

parseXElement :: MonadThrow m => ConduitM Event o m (Maybe Node)
parseXElement = tag (matching $ isJust . namePrefix)
    (\ n -> (,) n <$> manyA (optionalAttrRaw Just))
    (\ (name, attrs) -> return $ NodeElement $ Element name attrs [])
    where
        manyA x = do
            x' <- x
            case x' of
                Just y -> (y:) <$> manyA x
                Nothing -> pure []

opTag :: (MonadReader ParseState m, MonadThrow m)
      => ((Text, Maybe Text) -> ConduitM Event o m (Maybe a))
      -> ConduitM Event o m (Maybe a)
opTag = fmap join . tagNS "operation" (liftM2 (,) (requireAttr "name") (attr "parameterOrder"))

actionName = Name {nameLocalName = "Action", nameNamespace = Just "http://www.w3.org/2007/05/addressing/metadata", namePrefix = Just "wsam"}

parseInputMessage :: Parser (Maybe InputMessage)
parseInputMessage = tagNS "input"
    (liftM3 (,,) (attr "name") (textToName <$> requireAttr "message") (attr actionName)) -- AttrParser (Maybe Text, Name)
    (return . uncurry3 InputMessage)

parseOutputMessage :: Parser (Maybe OutputMessage)
parseOutputMessage = tagNS "output"
    (liftM3 (,,) (attr "name") (textToName <$> requireAttr "message") (attr actionName)) -- AttrParser (Maybe Text, Name)
    (return . uncurry3 OutputMessage)

uncurry3 :: (a -> b -> c -> d) -> (a,b,c) -> d
uncurry3 f (a,b,c) = f a b c

parseFault :: Parser (Maybe FaultMessage)
parseFault = tagNS "fault"
    (liftM2 (,) (requireAttr "name") (textToName <$> requireAttr "message"))
    (return . uncurry FaultMessage)

parseService :: Parser (Maybe WSDLService)
parseService = tagNS "service" (requireAttr "name")
    (\ n -> do
        xel <- many parseXElement
        ports <- many parsePort
        return $ WSDLService n ports xel)
         
parsePort :: Parser (Maybe WSDLPort)
parsePort = tagNS "port" (liftM2 (,) (requireAttr "name") (requireAttr "binding"))
    (\ (n,b) -> do
        xel <- many parseXElement
        return $ WSDLPort n (textToName b) (getAddress xel) xel)
         where getAddress xel = foldr (\el muri -> parseAddr el <|> muri) Nothing xel

parseAddr = (>>= parseURI . T.unpack) . readAddr 

readAddr :: Node -> Maybe Text
readAddr (NodeElement (Element (Name n ns pr) attrs nodes)) = do guardM (n == "address") ()
                                                                 foldr (\nc mb -> readElemAttr nc <|> mb) Nothing attrs
readElemAttr :: (Name, [Content]) -> Maybe Text
readElemAttr (Name nl ns pr, ct) = do guardM (nl == "location") ()
                                      readCntText ct
                                      
readCntText :: [Content] -> Maybe Text
readCntText [] = Nothing
readCntText (ContentText a : xs) = return a
readCntText (x:xs) = readCntText xs 

guardM = (\x -> (guard x >>) . Just)


textToName :: T.Text -> Name
textToName s = case fromString (T.unpack s) of
    Name {..} -> case T.split (== ':') nameLocalName of
                    [pre, x] -> Name x nameNamespace (Just pre)
                    [x] -> Name x nameNamespace Nothing
                    _ -> error "invalid name"

tagNS :: (MonadReader ParseState m, MonadThrow m)
      => Text
      -> AttrParser a
      -> (a -> ConduitM Event o m b)
      -> ConduitM Event o m (Maybe b)
tagNS t a p = do
    ns <- asks psDocumentNamespace
    tag (matching (\ n -> nameLocalName n == t && nameNamespace n == ns)) (const a) p
