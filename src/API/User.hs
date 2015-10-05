{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module API.User where

import API.Pagination
import User
import Util.JWT
import Util.Crypto

import Data.Int

import Data.Text

import Servant.API

type GetAll = QueryParam "offset" Int64
           :> Get '[JSON] (Headers '[Header "Link" Pagination] [User])

type UserAPI = GetAll
               -- GET /?offset=1 : Get a list of users
          
          :<|> ReqBody '[JSON] Registration 
            :> Post '[JSON] (Encoded Authorization)
               -- POST / : Register a user

          :<|> Capture "email" Email 
            :> Header "Authorization" (Token Authorization) 
            :> ReqBody '[JSON] User 
            :> Put '[] ()
               -- PUT /:email : Update user details

          :<|> Capture "email" Email
            :> Header "Authorization" (Token Authorization)
            :> Delete '[] () 
               -- DELETE /:email : Delete an account

          :<|> Capture "email" Email
            :> "token" 
            :> Header "Authorization" (Password Unhashed)
            :> Post '[JSON] (Encoded Authorization)
               -- POST /:email/token : Create a new authorization token (i.e. sign in)
