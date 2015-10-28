{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}

module UserSpec where

import App
import Server
import Util

import Test.Hspec
import Test.Hspec.Wai
import Test.Hspec.Wai.JSON

spec :: Spec
spec =
  with setup $
    describe "Users" $ do
        it "should create a user" $
            postJSON "/user" [json|{email:"test@gmail.com", password:"hunter2"}|] `shouldRespondWith` 201
        it "should not allow duplicate emails" $ do
            postJSON "/user" [json|{email:"test@gmail.com", password:"hunter2"}|] `shouldRespondWith` 201
            postJSON "/user" [json|{email:"test@gmail.com", password:"hunter2"}|] `shouldRespondWith` 409