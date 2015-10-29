{-# LANGUAGE OverloadedStrings #-}

module Util where

import App
import Server
import Server.Config

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS

import Data.Monoid

import Database.Neo4j
import qualified Database.Neo4j.Transactional.Cypher as C

import Control.Monad

import Network.HTTP.Types.Method
import Network.HTTP.Types.Header

import Network.Wai
import Network.Wai.Test hiding (request)

import Test.Hspec
import Test.Hspec.Wai
import Test.Hspec.Wai.JSON

clearDB :: Config -> IO ()
clearDB Config{neoConfig=NeoConfig host port mAuth} =
    case mAuth of
        Just auth -> liftIO . void $ withAuthConnection host port auth n

        Nothing -> liftIO . void $ withConnection host port n
    where n = C.runTransaction $ do C.cypher "MATCH n-[r]->() DELETE n, r" mempty
                                    C.cypher "MATCH n DELETE n" mempty

setup :: IO Application
setup  = do
    config <- parseConfig "test"
    clearDB config
    return $ app config

postJSON :: BS.ByteString -> LBS.ByteString -> WaiSession SResponse
postJSON url = request methodPost url [("Content-Type", "application/json")]

postJSONHeaders :: BS.ByteString -> [Header] -> LBS.ByteString -> WaiSession SResponse
postJSONHeaders url headers = request methodPost url (("Content-Type", "application/json"):headers)

putJSON :: BS.ByteString -> LBS.ByteString -> WaiSession SResponse
putJSON url = request methodPut url [("Content-Type", "application/json")]

putJSONHeaders :: BS.ByteString -> [Header] -> LBS.ByteString -> WaiSession SResponse
putJSONHeaders url headers = request methodPut url (("Content-Type", "application/json"):headers)


