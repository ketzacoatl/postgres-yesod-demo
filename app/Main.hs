{-# LANGUAGE OverloadedStrings #-}

import Application () -- for YesodDispatch instance
import Foundation
import Yesod.Core
import Database.Persist.Sql
import Database.Persist.MySQL (withMySQLPool, defaultConnectInfo, ConnectInfo (..) )

import Control.Monad.Logger

import Model (migrateAll)

--dbConnString :: ConnectInfo
--dbConnString = ConnectInfo
--  { connectHost = "localhost"
--  , connectPort = 3306
--  , connectUser = "mysql"
--  , connectDatabase = "app"
--  , connectPassword = "mysql"
--  }

openDbConnectionCount :: Int
openDbConnectionCount = 10

main :: IO ()
main =
  runNoLoggingT
    (withMySQLPool
      (defaultConnectInfo
        { connectHost = "127.0.0.1"
        , connectPort = 3306
        , connectUser = "mysql"
        , connectDatabase = "app"
        , connectPassword = "mysql"
        })
      openDbConnectionCount
      (\pool -> (runSqlPool (runMigration migrateAll) pool) >> liftIO (warp 3000 (App pool))))


