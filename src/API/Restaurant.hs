{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module API.Restaurant where

import Restaurant
import API.Pagination

import Data.Int

import Data.Text

import Servant.API

type GetAll = QueryParam "offset" Int64
           :> Get '[JSON] (Headers '[Header "Link" Pagination] [Restaurant])

type RestaurantAPI = GetAll
               -- GET /?offset=1 : Get a list of users