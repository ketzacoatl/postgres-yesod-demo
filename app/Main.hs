{-# LANGUAGE OverloadedStrings          #-}

import Application () -- for YesodDispatch instance
import Foundation
import Yesod.Core
import Database.Persist.Sql
import Database.Persist.Postgresql (withPostgresqlPool)

import Control.Monad.Logger
--import Control.Monad.Trans.Resource (runResourceT)

import Model (migrateAll)

pgConnString :: [Char]
pgConnString = "host=localhost port=5432 user=postgres dbname=app password=postgres"

openDbConnectionCount :: Int
openDbConnectionCount = 10

main :: IO ()
main =
  runNoLoggingT
    (withPostgresqlPool
      "host=localhost port=5432 user=postgres dbname=app password=postgres"
      openDbConnectionCount
      (\pool -> (runSqlPool (runMigration migrateAll) pool) >> liftIO (warp 3000 (App pool))))


