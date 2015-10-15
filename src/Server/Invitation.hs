{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Server.Invitation where

import API.Invitation
import API.Pagination
import User
import Invitation
import Restaurant
import Util.JWT
import Util.Neo
import Server.Config

import Control.Monad
import Control.Monad.Reader
import Control.Monad.Trans

import Data.Aeson

import qualified Data.ByteString.Lazy as LB

import Data.Int

import qualified Data.HashMap.Lazy as HM

import Data.Maybe

import Data.Text hiding (map)
import Data.Text.Encoding

import Database.Neo4j
import qualified Database.Neo4j.Transactional.Cypher as C

import Servant
import Servant.Server


invitationServer :: ServerT InvitationAPI ConfigM
invitationServer = getInvitations
              :<|> createInvitation
              :<|> acceptInvitation

getInvitations :: Maybe Int64 -> ConfigM (Headers '[Header "Link" Pagination] [Invitation])
getInvitations Nothing = getInvitations (Just 1)
getInvitations (Just offset) = runTransaction $ do
    [Only countVal] <- query "MATCH (i:Invitation) RETURN COUNT(i)" HM.empty
    invites <- query "MATCH (host:User)-[:CREATED]->(i:Invitation)-[:AT]->(r:Restaurant) \
                                   \ OPTIONAL MATCH (guest:User)-[:ACCEPTED]->i \
                                   \ RETURN ID(i), ID(host), host, ID(r), r, i, ID(guest), guest  \
                                   \ SKIP {offset} LIMIT 50" $ HM.fromList [("offset", C.newparam . (50 *) $ offset - 1)]

    let Success count = fromJSON countVal
        maxOffset = (count `div` 50) + 1

    return . addHeader (mkPagination offset maxOffset (Proxy :: Proxy GetAll) (Proxy :: Proxy InvitationAPI)) $ invites

createInvitation :: Maybe (Token Authorization) -> InvitationCreation -> ConfigM Invitation
createInvitation Nothing _ = errorOf err401
createInvitation (Just (Token token)) (InvitationCreation locId date) = do
  mAuth <- token <$> asks jwtSecret
  case mAuth of
    Nothing -> errorOf err401

    Just (Authorization authEmail) -> do
      result <- runNeo $ do
        [userNode] <- getNodesByLabelAndProperty "User" $ Just ("email" |: fromEmail authEmail)
        mRestaurant <- getNode (fromIntegral locId :: Integer)
        case mRestaurant of
          Nothing -> return $ Left err404

          Just restNode -> do inviteNode <- createNode $ HM.fromList [ "date" |: dropAround (== '"') (decodeUtf8 . LB.toStrict $ encode date)]
                              addLabels ["Invitation"] inviteNode
                              createRelationship "CREATED" HM.empty userNode inviteNode
                              createRelationship "AT" HM.empty inviteNode restNode

                              let  Just (userId :: Int64) = decodeStrict $ nodeId userNode
                                   Just (restId :: Int64) = decodeStrict $ nodeId restNode
                                   Just (inviteId :: Int) = decodeStrict $ nodeId inviteNode

                                   Just host = decode . encode $ HM.insert "id" (newval userId) (getNodeProperties userNode)
                                   Just restaurant = decode . encode $ HM.adjust (\(String hoursString) -> fromJust . decodeStrict . encodeUtf8 $ hoursString) "hours"
                                                      (fromJust . decode . encode $ HM.insert "id" (newval restId) (getNodeProperties restNode) :: HM.HashMap Text Value)

                              return . Right $ Invitation inviteId host restaurant date Nothing

      case result of
        Left err -> errorOf err

        Right created -> return created


acceptInvitation :: Int -> Maybe (Token Authorization) -> ConfigM ()
acceptInvitation = undefined