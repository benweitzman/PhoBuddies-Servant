{-# LANGUAGE OverloadedStrings #-}

module App where

import API
import Server

import Control.Monad.Trans

import qualified Data.ByteString.Char8 as BS

import Data.ConfigFile

import qualified Data.HashMap.Lazy as HM

import Data.Maybe

import Data.Text

import Database.Neo4j

import Network.Wai.Handler.Warp

import System.Environment
import System.Exit

import Web.JWT


parseConfig :: String -> IO Config
parseConfig runmode = do
  read <- readfile emptyCP "phobuddies.cfg"
  let config = do cp <- read
                  jwt <- secret . pack <$> get cp runmode "jwt_secret"
                  neoHost <- BS.pack <$> get cp runmode "neo_host"
                  neoPort <- get cp runmode "neo_port"
                  neoUser <- BS.pack <$> get cp runmode "neo_user"
                  neoPassword <- BS.pack <$> get cp runmode "neo_password"
                  return Config
                          { jwtSecret=jwt
                          , neoConfig=NeoConfig neoHost neoPort (Just (neoUser, neoPassword))
                          }
  case config of
    Left e -> putStrLn "Error parsing config file: " >> print e >> exitFailure

    Right c -> return c

main :: IO ()
main = do
  args <- getArgs
  case args of
    [runmode] -> do
      config <- parseConfig runmode
      Network.Wai.Handler.Warp.run 8080 (app config)

    _ -> putStrLn "Runmode required" >> exitFailure