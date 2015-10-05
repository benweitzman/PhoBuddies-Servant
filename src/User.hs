{-# LANGUAGE OverloadedStrings #-}

module User where

import qualified Data.Map as M

import Data.Aeson
import Data.Aeson.Types

import Data.Maybe

import Data.Monoid

import Data.Text hiding (map)

import Servant

import Util.JWT
import Util.Crypto

data Email = Email 
  { localPart :: Text
  , serverPart :: Text
  } deriving (Eq)

fromEmail :: Email -> Text
fromEmail (Email local server) = local <> "@" <> server

toEmail :: Text -> Maybe Email
toEmail email = case splitOn "@" email of
  ["", _] -> Nothing
  [_, ""] -> Nothing
  [local, server] -> Just $ Email local server
  _ -> Nothing

instance FromJSON Email where
  parseJSON (String v) = case toEmail v of
                          Just email -> return email
                          Nothing -> mempty
  parseJSON _ = mempty

instance FromText Email where
  fromText = toEmail

instance Show Email where
  show = unpack . fromEmail

data Registration = Registration
  { registrationEmail :: Email
  , registrationPassword :: Password Unhashed
  }

instance Show Registration where 
  show (Registration email pw) = "Registration{registrationEmail=" ++ show email ++ ", registrationPassword=" ++ show pw ++ "}"

instance FromJSON Registration where
  parseJSON (Object v) = Registration <$>
                         v .: "email" <*>
                         v .: "password"
  parseJSON _ = mempty


data Authorization = Authorization
  { authorizationEmail :: Email
  } deriving (Show, Eq)

instance Claimable Authorization where
  toMap (Authorization email) = M.fromList [("email", toJSON $ fromEmail email)]

  fromMap m = Authorization <$>
              (m .:. "email" >>= parseMaybe parseJSON)

data User = User
  { userID :: Int
  , userAge :: Maybe Int
  , userPhoto :: Maybe Text
  , userName :: Maybe Text
  } deriving (Show, Eq)

instance FromJSON User where
  parseJSON (Object v) = User <$>
                         v .: "id" <*>
                         v .:? "age" <*>
                         v .:? "photo" <*>
                         v .:? "name" 
  parseJSON _          = mempty

instance ToJSON User where
  toJSON (User id a p n) = 
        object $ "id" .= id :
                 map ("age" .=) (maybeToList a) ++
                 map ("photo" .=) (maybeToList p) ++
                 map ("name" .=) (maybeToList n)