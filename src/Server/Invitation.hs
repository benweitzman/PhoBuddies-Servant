{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Server.Invitation where

import API.Invitation
import API.Pagination
import User
import Invitation
import Util.JWT
import Server.Config

import Data.Aeson

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
    C.Result _ rows _ _ <- C.cypher "MATCH (host:User)-[CREATED]->(i:Invitation)-[AT]->(r:Restaurant) \
                                   \ OPTIONAL MATCH (guest:User)-[ACCEPTED]->i \
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

                invitationMap = case (HM.insert ("restaurant" :: Text) <$> restaurantMap) <*>
                                     ((HM.insert "host" <$> hostMap) <*>
                                     (HM.insert "id" inviteIdProp <$>
                                     (HM.insert "guest" guestMap <$> 
                                     HM.adjust (\(String dateString) -> fromJust . decode . encodeUtf8 $ LT.fromStrict timeString) "date" <$>   
                                     fromJSON inviteProp))) of
                                    Error _ -> Nothing

                                    Success x -> Just x
            in invitationMap >>= decode . encode) rows
        Success count = fromJSON countVal
        maxOffset = (count `div` 50) + 1

    return . addHeader (mkPagination offset maxOffset (Proxy :: Proxy GetAll) (Proxy :: Proxy InvitationAPI)) $ catMaybes mInvites

createInvitation :: Maybe (Token Authorization) -> Invitation -> ConfigM ()
createInvitation = undefined

acceptInvitation :: Int -> Maybe (Token Authorization) -> ConfigM ()
acceptInvitation = undefined