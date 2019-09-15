{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module CSV where

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

getCSVR :: Handler Html
getCSVR = do
    persons <- queryPersons

    defaultLayout $ do
        setTitle "Minimal Multifile"
        [whamlet|
            <p>#{show (persons)}
        |]
  where
    queryPersons :: Handler [Entity Person]
    queryPersons = runDB $ selectList [PersonName !=. ""] []
