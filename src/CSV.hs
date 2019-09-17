{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module CSV where

import           Data.Aeson (eitherDecode, FromJSON, ToJSON(..), Value, toJSON)
import           Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as BS
import           Data.Csv
  ( encodeDefaultOrderedByName
  , DefaultOrdered(headerOrder)
  , Header
  , namedRecord
  , ToNamedRecord(toNamedRecord)
  , (.=)
  )

import qualified Data.Csv as CSV
import qualified Data.Foldable as Foldable
import           Data.Vector (Vector)
import qualified Data.Vector as Vector
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Text.Encoding (encodeUtf8)
import           GHC.Generics (Generic)
import           Control.Monad.IO.Class (liftIO)


import Data.Aeson (encode)
import Data.Text (Text)

import Database.Persist
import Database.Persist.Sql
--import Database.Persist.Quasi
--import Database.Persist.TH
import Yesod.Persist.Core (runDB) --(Entity, runDB, selectList, (!=.))

import Foundation
import Yesod.Core (defaultLayout, setTitle, whamlet, Html(..)) --, Handler, Html, whamlet)

import Model

getCSVR :: Handler Html
getCSVR = do
    persons <- queryPersons
    --writeCsv persons
    let csvRecords = map processPerson persons

    defaultLayout $ do
        setTitle "select and write to CSV demo"
        [whamlet|
            <p>#{show (csvRecords)}
        |]
  where
    queryPersons :: Handler [Entity Person]
    queryPersons = runDB $ selectList [PersonName !=. ""] []

    --writeCsv :: [Entity Person] -> IO ()
    --writeCsv personList = do
    --    let records = map processPerson personList
    --    writeCsvPersonsToFile "./persons.csv" records

    processPerson :: (Entity Person) -> [PersonCsv]
    processPerson p = 
        let fName = personName (entityVal p)
            personJsonRaw = BS.fromStrict $ encodeUtf8 $ personJsonInfo (entityVal p)
            jsonData = processJsonData (eitherDecode personJsonRaw :: Either String [PersonJson])
            -- concatMap $ 
            csvRecords = map (generatePersonCSV fName) jsonData
            --csvRecords = Vector.fromList $ map (generatePersonCSV fName) jsonData
        in return csvRecords 
        
    -- a little error handling when processing the JSON input data
    processJsonData :: Either a b -> b
    processJsonData (Left _) = error "unable to parse data"
    processJsonData (Right x) = x

    generatePersonCSV :: Text -> PersonJson -> PersonCsv
    generatePersonCSV fname jp = PersonCsv
      { cPersonFullName = fname
      , cPersonName = name jp
      , cPersonAge = age jp
      }

    encodeCsvPersons :: Vector PersonCsv -> ByteString
    encodeCsvPersons = encodeDefaultOrderedByName . Foldable.toList

    writeCsvPersonsToFile :: FilePath -> Vector PersonCsv -> IO ()
    writeCsvPersonsToFile filePath =
      BS.writeFile filePath . encodeCsvPersons


csvHeader :: CSV.Header
csvHeader =
  Vector.fromList
    [ "Full Name"
    , "Name"
    , "Age"
    ]

data PersonJson = PersonJson
  { age :: Int
  , name :: Text
  } deriving (Show, Generic)

data PersonCsv = PersonCsv
  { cPersonFullName :: Text
  , cPersonName :: Text
  , cPersonAge :: Int
  } deriving (Show, Generic)

-- use generics
instance FromJSON PersonJson
instance ToJSON   PersonJson

instance CSV.DefaultOrdered PersonCsv where
  headerOrder _ =
    CSV.header
      [ "Full Name" -- .= cPersonFullName
      , "Name" -- .= cPersonName
      , "Age" -- .= cPersonAge
      ]

instance CSV.ToNamedRecord PersonCsv where
  toNamedRecord PersonCsv{..} =
    CSV.namedRecord
      [ "Full Name" .= cPersonFullName
      , "Name" .= cPersonName
      , "Age" .= cPersonAge
      ]
