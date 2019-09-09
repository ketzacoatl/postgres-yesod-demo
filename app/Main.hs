{-# LANGUAGE OverloadedStrings          #-}

import Application () -- for YesodDispatch instance
import Foundation
import Yesod.Core
import Database.Persist.Sql
import Database.Persist.MySQL (withMySQLPool)

import Control.Monad.Logger
--import Control.Monad.Trans.Resource (runResourceT)

import Model (migrateAll)

pgConnString :: [Char]
pgConnString = "host=localhost port=3306 user=mysql dbname=app password=mysql"

openDbConnectionCount :: Int
openDbConnectionCount = 10

main :: IO ()
main =
  runNoLoggingT
    (withMySQLPool
      "host=localhost port=3306 user=mysql dbname=app password=mysql"
      openDbConnectionCount
      (\pool -> (runSqlPool (runMigration migrateAll) pool) >> liftIO (warp 3000 (App pool))))


