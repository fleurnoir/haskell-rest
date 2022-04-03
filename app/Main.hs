{-# LANGUAGE UndecidableInstances #-}

module Main where

import BookStore.Models
import Database.Persist.Sql
import Control.Monad.Logger
import Database.Persist.Sqlite
import Happstack.Server
import Control.Monad
import Data.Pool
import Data.Int
import BookStore.Actions

main = do
    pool <- runStdoutLoggingT $ createSqlitePool "api.db" 5
    runSqlPool (runMigration migrateAll) pool
    simpleHTTP' (unpackSQLPart pool) nullConf server

server :: ServerAction Response
server = msum [
        dir "book" $ path getBookAction,
        dir "book" getBooksAction,
        dir "book" insertBookAction,
        dir "book" updateBookAction,
        dir "book" $ path deleteBookAction
    ]

unpackSQLPart :: Pool SqlBackend -> UnWebT (SqlPersistT (LoggingT IO)) a -> UnWebT IO a
unpackSQLPart pool sqlAction = runStdoutLoggingT $ runSqlPool sqlAction pool
instance (ToBackendKey SqlBackend a) => FromReqURI (Key a) where
    fromReqURI input = do
        id <- fromReqURI input :: Maybe Int64
        return $ toSqlKey id
