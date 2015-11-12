{-# LANGUAGE RankNTypes #-}

module Server.Config where

import Control.Monad.Reader
import Control.Monad.Trans.Either

import Data.ByteString

import qualified Data.HashMap.Lazy as HM

import Data.Text

import Database.Neo4j
import qualified Database.Neo4j.Transactional.Cypher as C

import Servant
import Servant.Server

import Web.JWT

data NeoConfig = NeoConfig
  { neoHost :: ByteString
  , neoPort :: Int
  , neoAuth :: Maybe (ByteString, ByteString)
  }

data Config = Config
  { jwtSecret :: Secret
  , neoConfig :: NeoConfig
  }

runNeo :: Neo4j a -> ConfigM a
runNeo n = do
  NeoConfig host port mAuth <- asks neoConfig
  case mAuth of
    Just auth -> liftIO $ withAuthConnection host port auth n
    Nothing -> liftIO $ withConnection host port n

runTransaction :: C.Transaction a -> ConfigM a
runTransaction t = do
  res <- runNeo (C.runTransaction t)
  case res of
    Right x -> return x
    Left (code, message) -> liftIO (print message) >> errorOf err500

runCypher :: Text -> C.Params -> ConfigM C.Result
runCypher query params = runTransaction (C.cypher query params)

errorOf :: ServantErr -> ConfigM a
errorOf = lift . left

type ConfigM = ReaderT Config (EitherT ServantErr IO)