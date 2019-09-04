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
    persons <- selectPersons "Heidi"
    defaultLayout $ do
        setTitle "Minimal Multifile"
        [whamlet|
            <p>
                <a href=@{AddR 5 7}>HTML addition
            <p>
                <a href=@{AddR 5 7}?_accept=application/json>JSON addition
        |]
  where
    selectPersons :: Text -> Handler [Entity Person]
    selectPersons t = runDB $ rawSql s [toPersistValue t]
      where s = "SELECT ?? FROM persons WHERE name = ? ORDER BY (role_id IS NULL) ASC, name DESC"
