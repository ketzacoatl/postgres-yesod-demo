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
--import Database.Persist.Quasi
--import Database.Persist.TH
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

    personA <- selectPersons "Heidi"
    personB <- selectPersons "Mike"
    defaultLayout $ do
        setTitle "Minimal Multifile"
        [whamlet|
            <p>#{show personA}
            <p>#{show personB}
            <p>
                <a href=@{AddR 5 7}>HTML addition
            <p>
                <a href=@{AddR 5 7}?_accept=application/json>JSON addition
        |]
  where
    selectPersons :: Text -> Handler [Entity Person]
    selectPersons t = runDB $ rawSql s [toPersistValue t]
      where s = "SELECT ?? FROM person WHERE name = ? ORDER BY id"
