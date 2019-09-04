{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE MultiParamTypeClasses   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE ViewPatterns      #-}
module Foundation where

import Database.Persist.Sql
import Yesod.Core
import Yesod.Persist.Core

-- | The foundation datatype for your application. This can be a good place to
-- keep settings and values requiring initialization before your application
-- starts running, such as database connections. Every handler will have
-- access to the data present here.
data App = App
    { --appSettings    :: AppSettings
--  , appStatic      :: Static -- ^ Settings for static file serving.
      appConnPool    :: ConnectionPool -- ^ Database connection pool.
--  , appHttpManager :: Manager
--  , appLogger      :: Logger
    }

mkYesodData "App" $(parseRoutesFile "routes")

instance Yesod App

instance YesodPersist App where
  type YesodPersistBackend App = SqlBackend

  runDB action = do
      master <- getYesod
      runSqlPool action $ appConnPool master

instance YesodPersistRunner App where
    getDBRunner = defaultGetDBRunner appConnPool

