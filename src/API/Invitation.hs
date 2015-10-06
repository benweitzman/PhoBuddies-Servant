{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module API.Invitation where

import API.Pagination
import Util.JWT
import User
import Invitation

import Data.ByteString.Char8
import Data.ByteString.Builder
import Data.ByteString.Conversion

import Data.Int

import Servant.API
import Servant.Utils.Links

type GetAll = QueryParam "offset" Int64 
           :> Get '[JSON] (Headers '[Header "Link" Pagination] [Invitation])

type InvitationAPI = GetAll
                     -- GET /?offset=1 : Get a list of invitations

                :<|> Header "Authorization" (Token Authorization)
                  :> ReqBody '[JSON] InvitationCreation
                  :> Post '[JSON] Invitation
                     -- POST / : Create an invitation

                :<|> Capture "id" Int 
                  :> "acceptance" 
                  :> Header "Authorization" (Token Authorization)
                  :> Post '[] ()
                     -- POST /:id/acceptance : Accept an invitation