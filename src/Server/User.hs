{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Server.User where

import API.User
import API.Pagination
import User
import Util.JWT
import Util.Crypto
import Server.Config

import Control.Applicative
import Control.Monad
import Control.Monad.Reader
import Control.Monad.Trans.Maybe

import Data.Aeson

import qualified Data.ByteString.Char8 as BSC

import Data.Int

import Data.Maybe

import Data.Text hiding (map)

import Database.Neo4j
import qualified Database.Neo4j.Transactional.Cypher as C

import qualified Data.HashMap.Lazy as HM

import Servant
import Servant.Server

import Web.JWT hiding (decode)

userServer :: ServerT UserAPI ConfigM
userServer = getAll
        :<|> register
--      :<|> getSingle
        :<|> updateUser
        :<|> deleteUser
        :<|> signIn

register :: Registration -> ConfigM (Encoded Authorization)
register (Registration email pw) = do
    mHashed <- hashPassword pw
    case mHashed of
        Nothing -> errorOf err500

        Just (HashedPW hashedPW) -> do
            created <- runNeo $ do 
                existing <- getNodesByLabelAndProperty "User" $ Just ("email" |: fromEmail email)
                case existing of
                    [] -> do
                        user <- createNode $ HM.fromList [ "email" |: fromEmail email
                                                         , "password" |: hashedPW
                                                         ]
                        addLabels ["User"] user
                        return True
                    _ -> return False
            unless created $ errorOf err409
            secret <- asks jwtSecret
            return $ Encoded (Authorization email) secret

signIn :: Email -> Maybe (Password Unhashed) -> ConfigM (Encoded Authorization)
signIn _ Nothing = errorOf err401
signIn email (Just unhashed) = do
    mHash <- runNeo $ do
        users <- getNodesByLabelAndProperty "User" $ Just ("email" |: fromEmail email)
        let mUser = listToMaybe users
        mPasswordProp <- runMaybeT $ MaybeT (return mUser) >>= (MaybeT . (flip getProperty) "password")
        case mPasswordProp of
            Just (ValueProperty (TextVal pwHash)) ->
                return . Just $ HashedPW pwHash

            _ -> return Nothing
    case mHash of 
        Nothing -> errorOf err403

        Just hashed -> do unless (verifyPassword hashed unhashed) $ errorOf err403
                          secret <- asks jwtSecret
                          return $ Encoded (Authorization email) secret  

updateUser :: Email -> Maybe (Token Authorization) -> User -> ConfigM ()
updateUser _ Nothing _ = errorOf err401
updateUser email (Just (Token token)) user = do
    mAuth <- token <$> asks jwtSecret
    case mAuth of 
        Just (Authorization authEmail) -> do
            unless (authEmail == email) $ errorOf err403
            let Just userProps = HM.delete "id" <$> (decode $ encode user)
            updated <- runNeo $ do
                userNodes <- getNodesByLabelAndProperty "User" $ Just ("email" |: fromEmail email)
                case userNodes of 
                    [userNode] -> do             
                        forM_ (HM.toList userProps) $ \(key, val) ->
                            setProperty userNode key val 
                        return True

                    [] -> return False

            unless updated $ errorOf err404
        Nothing -> errorOf err401

getAll :: Maybe Int64 -> ConfigM (Headers '[Header "Link" Pagination] [User])
getAll Nothing = getAll (Just 1)
getAll (Just offset) = runTransaction $ do
    C.Result _ [[countVal]] _ _ <- C.cypher "MATCH (u:User) RETURN COUNT(u)" HM.empty
    C.Result _ userRows _ _ <- C.cypher "MATCH (u:User) RETURN u, ID(u) SKIP {offset} LIMIT 50" $ HM.fromList [("offset", C.newparam . (50 *) $ offset - 1)]

    let mUsers = map (\[userProps, idProp] -> 
           let userMap = case HM.insert ("id" :: Text) idProp <$> fromJSON userProps of
                            Error _ -> Nothing

                            Success x -> Just x
            in userMap >>= decode . encode) userRows
        Success count = fromJSON countVal
        maxOffset = (count `div` 50) + 1

    return . addHeader (mkPagination offset maxOffset (Proxy :: Proxy GetAll) (Proxy :: Proxy UserAPI)) $ catMaybes mUsers

{-
getSingle :: Email -> ConfigM User
getSingle email = do
    user <- runNeo $ do
        userNodes <- getNodesByLabelAndProperty "User" $ Just ("email" |: fromEmail email)
        userProperties <- forM userNodes getProperties
        case userProperties of 
            [user] -> return . decode $ encode user

            _ -> return Nothing
    case user of 
        Just x -> return x

        Nothing -> errorOf err404
-}    

deleteUser :: Email -> Maybe (Token Authorization) -> ConfigM ()
deleteUser _ Nothing = errorOf err401
deleteUser email (Just (Token token)) = do
    mAuth <- token <$> asks jwtSecret
    case mAuth of 
        Nothing -> errorOf err401

        Just (Authorization authEmail) -> do
            unless (authEmail == email) $ errorOf err403
            deleted <- runNeo $ do
                userNodes <- getNodesByLabelAndProperty "User" $ Just ("email" |: fromEmail email)
                case userNodes of 
                    [userNode] -> deleteNode userNode >> return True
                    
                    _ -> return False
            unless deleted $ errorOf err404
