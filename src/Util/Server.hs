module Util.Server where

import Control.Monad.Reader

import Server.Config
import Util.JWT

import Servant
import Servant.Server

import Web.JWT

requireToken :: Maybe (Token a) -> ConfigM a
requireToken Nothing = errorOf err401
requireToken (Just (Token token)) = do
    mAuth <- token <$> asks jwtSecret
    case mAuth of
        Just x -> return x

        Nothing -> errorOf err401