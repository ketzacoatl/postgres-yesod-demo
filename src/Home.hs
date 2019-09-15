{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Home where

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

getHomeR :: Handler Html
getHomeR = do
    personA <- selectPersons "Chris"
    personB <- selectPersons "Heidi"
    let personAList = map entityVal personA
    let personBList = map entityVal personB
    let pA = head personAList
    let pAInfo = PersonInfoJSON $ personJsonInfo pA
    let pAName = pInfoName $ pAInfo
    let pB = head personBList
    let pBInfo = personJsonInfo pB
    let pBName = pInfoName $ PersonInfoJSON pBInfo

    defaultLayout $ do
        setTitle "Minimal Multifile"
        [whamlet|
            <p>#{show (personName pA)}
            <p>#{show (pAInfo)}
            <p>#{show (personJsonInfo pA)}
            <p>#{show (pAName)}

            <p>#{show (personName pB)}
            <p>#{show (pBInfo)}
            <p>#{show (personJsonInfo pB)}
            <p>#{show (pBName)}
        |]
  where
    selectPersons :: Text -> Handler [Entity Person]
    selectPersons t = runDB $ rawSql s [toPersistValue t]
      where s = "SELECT ?? FROM person WHERE name = ? ORDER BY id"
