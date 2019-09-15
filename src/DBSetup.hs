{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TemplateHaskell   #-}
module DBSetup where

import Data.Aeson (encode)
import Data.Text (Text)

import Database.Persist
import Database.Persist.Sql
--import Database.Persist.Quasi
--import Database.Persist.TH
import Yesod.Persist.Core

import Foundation
import Yesod.Core

import Model

getDBSetupR :: Handler Html
getDBSetupR = do
    persons <- runDB $ do
        pA <- insert $ Person "Chris" "[{\"name\": \"Chris\", \"age\": 75},{\"name\": \"chris\", \"age\": 25},{\"name\": \"kris\", \"age\": 17}]"
        pB <- insert $ Person "Mike"  "[{\"name\": \"Mike\", \"age\": 88},{\"name\": \"mikie\", \"age\": 8}]"
        pC <- insert $ Person "Heidi" "[{\"name\": \"Heidi\", \"age\": 47},{\"name\": \"hidy\", \"age\": 57},{\"age\": 37, name\": \"H\"}]"
        pure () -- why is this necessary for the do block? is this ok, or is there another/better way?

    defaultLayout $ do
        setTitle "Database Setup"
        [whamlet|
            wrote some db entries!
        |]
