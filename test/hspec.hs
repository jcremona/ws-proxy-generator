{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

import Control.Exception
import Control.Monad        hiding (forM_)
import Data.ByteString.Lazy (fromStrict)
import Data.Either
import Data.FileEmbed
import Data.Foldable        (forM_)
import Network.URI
import Test.Hspec
import Text.XML.WSDL.Parser
import Text.XML.WSDL.Types
---------------------------
import Model.WSDL2Model
import Model.ProxyModel
import Control.Monad.Reader

parsedWsdl :: Either SomeException WSDL
parsedWsdl = parseLBS $ fromStrict $(embedFile "/home/jcremona/ws-proxy-generator/test/hello.wsdl")

libi :: [Port]
libi = case parsedWsdl of
           Right wsdl -> runReader binding wsdl
           _ -> []

main :: IO ()
main = forM_ parsedWsdl $ \ wsdl -> hspec $ do
    describe "WSDL" $ it "Show" $ show wsdl `shouldStartWith` "WSDL"

    describe "parse" $ do
        it "targetNamespace" $
            targetNamespace wsdl `shouldBe` parseURI "http://www.examples.com/wsdl/HelloService.wsdl"
        it "types" $
            types wsdl `shouldBe` Just (WSDLTypes [Schema] [])
        describe "messages" $ do
            let firstMessage = head (messages wsdl)
            it "names" $
                wsdlMessageName firstMessage `shouldBe` "SayHelloRequest"
            describe "parts" $ do
                let firstPart = head (wsdlMessageParts firstMessage)
                it "names" $
                    wsdlMessagePartName firstPart `shouldBe` "firstName"
                it "types" $
                    wsdlMessagePartType firstPart `shouldBe` Just "string"
