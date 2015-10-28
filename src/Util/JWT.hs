{-# LANGUAGE OverloadedStrings #-}

module Util.JWT where

import Control.Monad

import Data.Aeson hiding (decode)

import Data.Map

import Data.Monoid

import Data.Text

import Servant

import Web.JWT

jwtAlgorithm :: Algorithm
jwtAlgorithm = HS256

class Claimable a where
  toMap :: a -> Map Text Value
  fromMap :: Map Text Value -> Maybe a

map .:. key = Data.Map.lookup key map

encodeToken :: Claimable a => Secret -> a -> Text
encodeToken secret claims = encodeSigned jwtAlgorithm secret cs
  where cs = def { unregisteredClaims = toMap claims }

decodeToken :: Claimable a => Secret -> Text -> Maybe a
decodeToken secret encoded = do
  jwt <- decode encoded

  -- prevent the client from modifying the jwt and changing the algorithm to "none",
  -- which is valid with any signature.
  givenAlgo <- alg $ header jwt
  unless (givenAlgo == jwtAlgorithm) $ return ()
  verify secret jwt >>= fromMap . unregisteredClaims . claims

data Token a = Token (Secret -> Maybe a)

data Encoded a = Encoded a Secret

instance Claimable a => FromText (Token a) where
  fromText header = case splitOn " " header of
                      ["Bearer", encoded] -> Just $ Token (`decodeToken` encoded)
                      _ -> Nothing

instance Claimable a => ToJSON (Encoded a) where
  toJSON (Encoded clm sec) =
        object ["token" .= encodeToken sec clm]