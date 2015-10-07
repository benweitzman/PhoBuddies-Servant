{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Restaurant where

import qualified Data.Map as M

import Control.Monad

import Data.Aeson
import Data.Aeson.Types

import Data.Monoid

import Data.Text

import Data.Time (TimeOfDay(..), LocalTime(..), Day(..))

import GHC.Generics

import Servant

data WeekDay = Monday
             | Tuesday
             | Wednesday
             | Thursday
             | Friday
             | Saturday
             | Sunday deriving (Show, Read, Eq, Ord)

instance ToJSON WeekDay where
  toJSON = String . pack . show

instance FromJSON WeekDay where
  parseJSON (String t) = pure . read $ unpack t
  parseJSON _ = mzero            

data OpenHours = OpenHours 
    { openTime :: TimeOfDay 
    , closeTime :: TimeOfDay
    } deriving (Show, Eq, Generic)

instance FromJSON OpenHours

instance ToJSON OpenHours

data Hours = Hours (M.Map WeekDay OpenHours) deriving (Show, Eq)

instance ToJSON Hours where
  toJSON (Hours m) = toJSON $ M.mapKeys show m

instance FromJSON Hours where
    parseJSON a = Hours . M.mapKeys read <$> parseJSON a

data Restaurant = Restaurant
  { restaurantId :: Int
  , restaurantName :: Text
  , description :: Text
  , address :: Text
  , latitude :: Double
  , longitude :: Double
  , restaurantPhoto :: Text
  , hours :: Hours
  } deriving (Generic, Show)

instance FromJSON Restaurant where
    parseJSON (Object v) = Restaurant <$>
                           v .: "id" <*>
                           v .: "name" <*>
                           v .: "description" <*>
                           v .: "address" <*>
                           v .: "latitude" <*> 
                           v .: "longitude" <*>
                           v .: "photo" <*> 
                           v .: "hours"
    parseJSON _ = mempty

instance ToJSON Restaurant