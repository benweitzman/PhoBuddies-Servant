{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Invitation where

import User
import Restaurant
import Util.Neo

import Data.Aeson
import Data.Aeson.Types

import Data.Maybe

import Data.Monoid

import Data.Text hiding (map)

import Data.Time.Clock (UTCTime)
import Data.Time (TimeOfDay, LocalTime)

import GHC.Generics

import Servant

data Invitation = Invitation
  { invitationId :: Int
  , host :: User
  , location :: Restaurant
  , date :: LocalTime
  , guest :: Maybe User
  } deriving (Show)

instance ToJSON Invitation where
  toJSON (Invitation id host location date guest) =
        object $ [ "id" .= id
                 , "host" .= host
                 , "location" .= location
                 , "date" .= date
                 ] ++
                 map ("guest" .=) (maybeToList guest)

instance FromJSON Invitation where
  parseJSON (Object v) = Invitation <$>
                         v .: "id" <*>
                         v .: "host" <*>
                         v .: "location" <*>
                         v .: "date" <*>
                         v .: "guest"
  parseJSON _ = mempty

data InvitationCreation = InvitationCreation
  { locationId :: Int
  , date' :: LocalTime
  } deriving (Show, Eq)

instance FromJSON InvitationCreation where
  parseJSON (Object v) = InvitationCreation <$>
                         v .: "locationId" <*>
                         v .: "date"
  parseJSON _ = mempty

instance FromRow Invitation where
  fromRow = Invitation <$>
            field <*>
            fromRow <*>
            fromRow <*>
            extract "date" <*>
            fromRow