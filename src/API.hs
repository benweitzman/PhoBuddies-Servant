{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module API where

import API.User
import API.Invitation
import API.Restaurant

import Servant.API

type API = "user" :> UserAPI
      :<|> "invitation" :> InvitationAPI
      :<|> "restaurant" :> RestaurantAPI