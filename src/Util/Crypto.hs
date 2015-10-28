{-# LANGUAGE EmptyDataDecls     #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE GADTs              #-}
{-# LANGUAGE OverloadedStrings  #-}

module Util.Crypto
    ( Hashed
    , Unhashed
    , Password (HashedPW)
    , hashPassword
    , verifyPassword
    )
where

import Control.Monad
import Control.Monad.Trans

import Crypto.BCrypt hiding (hashPassword)

import Data.Aeson

import Data.Text
import Data.Text.Encoding

import Servant

data Hashed

data Unhashed

data Password a where
    HashedPW :: Text -> Password Hashed
    UnhashedPW :: Text -> Password Unhashed

instance Show (Password Unhashed) where
    show _ = "<password obscured>"

instance Show (Password Hashed) where
    show (HashedPW x) = show x

hashPassword :: MonadIO m => Password Unhashed -> m (Maybe (Password Hashed))
hashPassword (UnhashedPW password) = liftIO $
    fmap (HashedPW . decodeUtf8) <$>
    hashPasswordUsingPolicy slowerBcryptHashingPolicy (encodeUtf8 password)

verifyPassword :: Password Hashed -> Password Unhashed -> Bool
verifyPassword (HashedPW hash) (UnhashedPW password) = validatePassword (encodeUtf8 hash) (encodeUtf8 password)

instance FromJSON (Password Unhashed) where
  parseJSON (String v) = return $ UnhashedPW v
  parseJSON _ = mempty

instance FromText (Password Unhashed) where
  fromText header = case splitOn " " header of
                      ["Simple", pw] -> Just (UnhashedPW pw)
                      _ -> Nothing