{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}

module UserSpec where

import App
import Server
import User
import Util

import Data.Aeson hiding (json)

import qualified Data.ByteString.Char8 as BS

import qualified Data.Map as M

import Data.Maybe

import Data.Monoid

import Data.Text

import Network.Wai.Test

import Test.Hspec
import Test.Hspec.Wai
import Test.Hspec.Wai.JSON

import Test.QuickCheck

spec :: Spec
spec = do
    with setup $
        describe "server" $ do
            it "should create a user" $
                postJSON "/user" [json|{email:"test@gmail.com", password:"hunter2"}|] `shouldRespondWith` 201
            it "should not allow malformed registrations" $
                postJSON "/user" [json|[1,2,3]|] `shouldRespondWith` 400
            it "should not allow duplicate emails" $ do
                postJSON "/user" [json|{email:"test@gmail.com", password:"hunter2"}|] `shouldRespondWith` 201
                postJSON "/user" [json|{email:"test@gmail.com", password:"hunter2"}|] `shouldRespondWith` 409
            it "should allow users to login" $ do
                postJSON "/user" [json|{email:"test@gmail.com", password:"hunter2"}|] `shouldRespondWith` 201
                postJSONHeaders "/user/test@gmail.com/token" [("Authorization", "Simple hunter2")] "" `shouldRespondWith` 201
            it "should prevent unauthenticated users from logging in" $ do
                postJSON "/user" [json|{email:"test@gmail.com", password:"hunter2"}|] `shouldRespondWith` 201
                postJSON "/user/test@gmail.com/token" "" `shouldRespondWith` 401
                postJSONHeaders "/user/test@gmail.com/token" [("Authorization", "Simple hunter3")] "" `shouldRespondWith` 403
            it "should update users" $ do
                resp <- postJSON "/user" [json|{email:"test@gmail.com", password:"hunter2"}|]
                let tokenJSON = simpleBody resp
                    token = BS.pack . fromMaybe ("dummy token" :: String) $ decode tokenJSON >>= M.lookup ("token" :: String)
                pure resp `shouldRespondWith` 201
                resp' <- putJSONHeaders "/user/test@gmail.com" [("Authorization", "Bearer " <> token)] [json|{age:23}|]
                pure resp `shouldRespondWith` 201

    describe "email" $ do
        it "should convert between text and email" $
            property $ \email@(Email local server) ->
                not ("@" `isInfixOf` local) && not ("@" `isInfixOf` server) ==>
                toEmail (fromEmail email) == Just email
        it "should fail to convert non-emails" $ do
            toEmail "" `shouldBe` Nothing
            toEmail "@" `shouldBe` Nothing
            toEmail "a@" `shouldBe` Nothing
            toEmail "@a" `shouldBe` Nothing
            toEmail "a" `shouldBe` Nothing
        it "decodes JSON" $ do
            decode "" `shouldBe` (Nothing :: Maybe Email)
            decode "\"ben@test\"" `shouldBe` Just (Email "ben" "test")
            decode "\"\"" `shouldBe` (Nothing :: Maybe Email)
            decode "{\"email\":\"ben@test\"}" `shouldBe` (Nothing :: Maybe Email)
        it "should show" $
            property $ \email@(Email local server) ->
                not ("@" `isInfixOf` local) && not ("@" `isInfixOf` server) ==>
                show email == unpack local ++ "@" ++ unpack server

instance Arbitrary Email where
    arbitrary = do
        NonEmpty local <- arbitrary
        NonEmpty server <- arbitrary
        return $ Email (pack local) (pack server)
