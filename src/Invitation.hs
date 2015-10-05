{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Invitation where

import User
import Restaurant

import Data.Aeson
import Data.Aeson.Types

import Data.Monoid

import Data.Text

import Data.Time.Clock (UTCTime)
import Data.Time (TimeOfDay, LocalTime)

import GHC.Generics

import Servant

data Invitation = Invitation
  { invitationId :: Int
  , host :: User
  , location :: Restaurant
  , time :: LocalTime
  , guest :: Maybe User
  } deriving (Generic, Show)

instance FromJSON Invitation

instance ToJSON Invitation