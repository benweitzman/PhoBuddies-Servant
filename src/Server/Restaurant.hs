{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Server.Restaurant where

import API.Restaurant
import API.Pagination
import Restaurant
import Server.Config
import Util.Neo

import Control.Applicative
import Control.Monad
import Control.Monad.Reader
import Control.Monad.Trans.Maybe

import Data.Aeson

import Data.Int

import Data.Maybe

import Data.Text hiding (map)
import qualified Data.Text.Lazy as LT
import Data.Text.Lazy.Encoding

import Database.Neo4j
import qualified Database.Neo4j.Transactional.Cypher as C

import qualified Data.HashMap.Lazy as HM

import Servant
import Servant.Server

restaurantServer :: ServerT RestaurantAPI ConfigM
restaurantServer = getAll

getAll :: Maybe Int64 -> ConfigM (Headers '[Header "Link" Pagination] [Restaurant])
getAll Nothing = getAll (Just 1)
getAll (Just offset) = runTransaction $ do
    [Only countVal] <- query "MATCH (r:Restaurant) RETURN COUNT(r)" HM.empty
    rests <- query "MATCH (r:Restaurant) RETURN ID(r), r SKIP {offset} LIMIT 50" $ HM.fromList [("offset", C.newparam . (50 *) $ offset - 1)]
    let Success count = fromJSON countVal
        maxOffset = (count `div` 50) + 1

    return . addHeader (mkPagination offset maxOffset (Proxy :: Proxy GetAll) (Proxy :: Proxy RestaurantAPI)) $ rests