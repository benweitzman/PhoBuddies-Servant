module Server (app
              , module Server.Config
              ) 
where

import API
import Server.User
import Server.Invitation
import Server.Config
import Server.Restaurant

import Data.Proxy

import Servant
import Servant.Server

import Network.Wai

myApi :: Proxy API
myApi = Proxy

server :: Config -> Server API
server c = enter (runReaderTNat c) (userServer :<|> invitationServer :<|> restaurantServer)

app :: Config -> Application
app c = serve myApi (server c)

