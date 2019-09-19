{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Home where

import Data.Text (Text)

import Database.Persist
import Database.Persist.Sql
import Yesod.Persist.Core

import Foundation
import Yesod.Core

import Model

getHomeR :: Handler Html
getHomeR = do
    persons <- runDB $ do
        pA <- insert $ Person "Heidi"
        pB <- insert $ Person "Mike"
        pC <- insert $ Person "Chris"
        pure ()
    personA <- selectPersons "Chris"
    personB <- selectPersons "Heidi"
    let personAList = map entityVal personA
    let personBList = map entityVal personB

    defaultLayout $ do
        setTitle "Minimal Multifile"
        [whamlet|
            <p>#{show (personName $ head personAList)}
            <p>#{show (personName $ head personBList)}
        |]
  where
    selectPersons :: Text -> Handler [Entity Person]
    selectPersons t = runDB $ rawSql s [toPersistValue t]
      where s = "SELECT ?? FROM person WHERE name = ? ORDER BY id"
