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
import qualified Data.Text.Lazy as LT
import Data.Text.Lazy.Encoding

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
    C.Result _ [[countVal]] _ _ <- C.cypher "MATCH (i:Invitation) RETURN COUNT(i)" HM.empty
    C.Result _ rows _ _ <- C.cypher "MATCH (host:User)-[:CREATED]->(i:Invitation)-[:AT]->(r:Restaurant) \
                                   \ OPTIONAL MATCH (guest:User)-[:ACCEPTED]->i \
                                   \ RETURN host, ID(host), guest, ID(guest), i, ID(i), r, ID(r) \
                                   \ SKIP {offset} LIMIT 50" $ HM.fromList [("offset", C.newparam . (50 *) $ offset - 1)]
    let mInvites = map (\[hostProp, hostIdProp, guestProp, guestIdProp, inviteProp, inviteIdProp, restProp, restIdProp] ->
            let hostMap = toJSON <$> HM.insert ("id" :: Text) hostIdProp <$> fromJSON hostProp
                guestMap = toJSON $ case HM.insert ("id" :: Text) guestIdProp <$> fromJSON guestProp of 
                                      Error _ -> Nothing

                                      Success x -> Just x
                restaurantMap = toJSON <$> HM.adjust (\(String hoursString) -> fromJust . decode . encodeUtf8 $ LT.fromStrict hoursString) "hours" <$>
                                           HM.insert ("id" :: Text) restIdProp <$> 
                                           fromJSON restProp
                invitationMap = case (HM.insert ("location" :: Text) <$> restaurantMap) <*>
                                     ((HM.insert "host" <$> hostMap) <*>
                                     (HM.insert "id" inviteIdProp <$>
                                     (HM.insert "guest" guestMap <$> 
                                     fromJSON inviteProp))) of
                                    Error _ -> Nothing

                                    Success x -> Just x
            in invitationMap >>= decode . encode) rows
        Success count = fromJSON countVal
        maxOffset = (count `div` 50) + 1

    return . addHeader (mkPagination offset maxOffset (Proxy :: Proxy GetAll) (Proxy :: Proxy InvitationAPI)) $ catMaybes mInvites

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

          Just restNode -> do inviteNode <- createNode $ HM.fromList [ "date" |: dropAround (== '"') (LT.toStrict (decodeUtf8 (encode date)))]
                              addLabels ["Invitation"] inviteNode
                              createRelationship "CREATED" HM.empty userNode inviteNode
                              createRelationship "AT" HM.empty inviteNode restNode

                              liftIO $ print restNode
                              let  Just (userId :: Int64) = decode . LB.fromStrict $ nodeId userNode
                                   Just (restId :: Int64) = decode . LB.fromStrict $ nodeId restNode
                                   Just (inviteId :: Int) = decode . LB.fromStrict $ nodeId inviteNode

                                   Just host = decode . encode $ HM.insert "id" (newval userId) (getNodeProperties userNode)
                                   Just restaurant = decode . encode $ HM.adjust (\(String hoursString) -> fromJust . decode . encodeUtf8 $ LT.fromStrict hoursString) "hours"
                                                      (fromJust . decode . encode $ HM.insert "id" (newval restId) (getNodeProperties restNode) :: HM.HashMap Text Value)

                              return . Right $ Invitation inviteId host restaurant date Nothing

      case result of
        Left err -> errorOf err

        Right created -> return created


acceptInvitation :: Int -> Maybe (Token Authorization) -> ConfigM ()
acceptInvitation = undefined