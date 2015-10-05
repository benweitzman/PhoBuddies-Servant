{-# LANGUAGE OverloadedStrings #-}

module Main where

import API
import Server

import Control.Monad.Trans

import qualified Data.HashMap.Lazy as HM

import Data.Maybe

import Data.Text

import Database.Neo4j

import Network.Wai.Handler.Warp

import System.Environment

import Web.JWT

main :: IO ()
main = do
  jwt <- (secret . pack . fromMaybe "this is my jwt secret") <$> lookupEnv "JWT_SECRET"
  let c = Config 
           { jwtSecret=jwt
           , neoConfig=NeoConfig "192.168.99.100" 8474 (Just ("neo4j", "password"))
           }
  Network.Wai.Handler.Warp.run 8080 (app c)