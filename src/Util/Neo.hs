{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE LambdaCase #-}

module Util.Neo where

import Control.Applicative
import Control.Monad
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Except

import Data.Aeson

import qualified Data.HashMap.Strict as HM

import Data.Monoid

import Data.Text hiding (length)
import qualified Data.Text.Lazy as LT
import Data.Text.Lazy.Encoding

import Database.Neo4j
import qualified Database.Neo4j.Transactional.Cypher as C

data Row = Row
  { columns :: [Text]
  , values :: [Value]
  }

newtype RowParserT m a = RP
  { unRP :: ReaderT Row (StateT Int (ExceptT Text m)) a
  } deriving ( Functor, Applicative, Alternative, Monad,
               MonadReader Row, MonadState Int, MonadError Text)

parseRow :: Monad m => Row -> RowParserT m a -> m (Either Text a)
parseRow row parser = runExceptT (evalStateT (runReaderT (unRP parser) row) 0)

query :: FromRow a => Text -> C.Params -> C.Transaction [a]
query cypher params = do
  C.Result cols rows _ _ <- C.cypher cypher params
  forM rows (\row -> parseRow (Row cols row) fromRow >>= \case Right x -> return x
                                                               Left err -> throwError ("RowParse", err))

execute :: Text -> C.Params -> C.Transaction ()
execute cypher params = void $ C.cypher cypher params

class FromRow a where
    fromRow :: Monad m => RowParserT m a

currentValue :: Monad m => RowParserT m Value
currentValue = do
  idx <- get
  vals <- asks values
  if idx >= length vals
    then throwError "Index out of bounds"
    else return $ vals !! idx

currentColumn :: Monad m => RowParserT m Text
currentColumn = do
  idx <- get
  cols <- asks columns
  if idx >= length cols
    then throwError "Index out of bounds"
    else return $ cols !! idx

field :: (FromJSON a, Monad m) => RowParserT m a
field = do
  val <- currentValue
  case fromJSON val of
    Error err -> throwError $ "Unable to parse field JSON: " <> pack err

    Success x -> modify (+1) >> return x

extract :: (FromJSON a, Monad m) => Text -> RowParserT m a
extract key = do
  json <- currentValue
  case json of
    Object m ->
      case HM.lookup key m of
        Just val@(String valString) ->
          case fromJSON val of
            Error err -> case decode . encodeUtf8 $ LT.fromStrict valString of
                           Just x -> modify (+1) >> return x

                           Nothing -> throwError $ "Unable to parse value JSON: " <> pack err

            Success x -> modify (+1) >> return x

        Just val ->
          case fromJSON val of
            Error err -> throwError $ "Unable to parse value JSON: " <> pack err

            Success x -> modify (+1) >> return x

        Nothing -> throwError $ "Could not find key " <> key <> " in field"

    x -> throwError $ "Extracting " <> key <> " expected an object, got " <> pack (show x)

attempt :: Monad m => FromJSON a => Text -> RowParserT m (Maybe a)
attempt key = do
  json <- currentValue
  case json of
    Object m ->
      case HM.lookup key m of
        Just val@(String valString) ->
          case fromJSON val of
            Error err -> case decode . encodeUtf8 $ LT.fromStrict valString of
                           Just x -> modify (+1) >> return x

                           Nothing -> throwError $ "Unable to parse value JSON: " <> pack err

            Success x -> modify (+1) >> return x

        Just val ->
          case fromJSON val of
            Error err -> throwError $ "Unable to parse value JSON: " <> pack err

            Success x -> modify (+1) >> return (Just x)

        Nothing -> modify (+1) >> return Nothing

    Null -> modify (+1) >> return Nothing

    x -> throwError $ "Attempt expected an object, got " <> pack (show x)

infixl 4 <->

(<->) :: Monad m => RowParserT m (a -> b) -> RowParserT m a -> RowParserT m b
rf <-> ra = do
  f <- rf
  modify (\x -> x - 1)
  a <- ra
  return $ f a

---------------
-- Instances --
---------------

newtype Only a = Only
  { fromOnly :: a
  } deriving (Eq, Show, Functor)

instance FromJSON a => FromRow (Only a) where
  fromRow = Only <$> field

infixr 3 :.

data h :. t = h :. t

instance (FromRow h, FromRow t) => FromRow (h :. t) where
  fromRow = (:.) <$> fromRow <*> fromRow

instance FromRow a => FromRow (Maybe a) where
  fromRow = catchError (Just <$> fromRow) $ \e -> modify (+2) >> return Nothing

instance FromJSON a => FromRow [a] where
  fromRow = some field

instance (FromJSON a, FromJSON b) => FromRow (a, b) where
  fromRow = (,) <$> field <*> field

instance (FromJSON a, FromJSON b, FromJSON c) => FromRow (a, b, c) where
  fromRow = (,,) <$> field <*> field <*> field

instance (FromJSON a, FromJSON b, FromJSON c, FromJSON d) => FromRow (a, b, c, d) where
  fromRow = (,,,) <$> field <*> field <*> field <*> field

instance (FromJSON a, FromJSON b, FromJSON c, FromJSON d, FromJSON e) => FromRow (a, b, c, d, e) where
  fromRow = (,,,,) <$> field <*> field <*> field <*> field <*> field