{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE QuasiQuotes                #-}
module Model where

import GHC.Generics (Generic)

import Data.Aeson

import Database.Persist.Quasi

import Data.Text (Text)

import Database.Persist
import Database.Persist.Sql
import Database.Persist.Quasi
import Database.Persist.TH
import Yesod.Persist.Core


-- You can define all of your database entities in the entities file.
-- You can find more information on persistent and how to declare entities
-- at:
-- http://www.yesodweb.com/book/persistent/
--share [mkPersist sqlSettings, mkMigrate "migrateAll"]
--    $(persistFileWith lowerCaseSettings "config/models")

data PersonInfoJSON = PersonInfoJSON {
    pInfoName :: Text
  } deriving (Show, Generic)

instance FromJSON PersonInfoJSON
instance ToJSON   PersonInfoJSON

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Person
    name Text
    jsonInfo Text
    deriving Show
|]

