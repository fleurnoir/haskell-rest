{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}

module Main where

import BookStore.Models
import Database.Persist.Sql
import Control.Monad.Logger
import Database.Persist.Sqlite
import Happstack.Server
import Control.Monad
import Data.Pool
import BookStore.Utils (jsonResponse, jsonMaybeResponse, getRequestBody)
import Control.Monad.IO.Class
import Data.Aeson
import BookStore.Repository
import Data.Int
import Happstack.Server.Internal.Monads (ServerPartT(ServerPartT))
import qualified Data.Text as T

main = do
    pool <- runStdoutLoggingT $ createSqlitePool "api.db" 5
    runSqlPool (runMigration migrateAll) pool
    simpleHTTP nullConf $ server pool

runSQL :: ToJSON a 
    => SqlPersistT (LoggingT IO) a 
    -> (a -> ServerPartT IO Response) 
    -> Pool SqlBackend 
    -> ServerPartT IO Response
runSQL action buildResponse pool = do
    result <- liftIO $ runStdoutLoggingT $ runSqlPool action pool
    buildResponse result

server :: Pool SqlBackend -> ServerPartT IO Response
server pool = msum [
        dir "book" $ path $ \id -> do
            method GET
            runSQL (getBook id) jsonMaybeResponse pool,
        dir "book" $ do
            method GET
            runSQL getBooks jsonResponse pool,
        dir "book" $ do
            method POST
            body <- getRequestBody :: ServerPartT IO (Maybe Book)
            case body of
                Nothing -> badRequest $ toResponse ("Invalid body" :: T.Text)
                Just book -> runSQL (insertBook book) jsonResponse pool
    ]

instance (ToBackendKey SqlBackend a) => FromReqURI (Key a) where
    fromReqURI input = do
        id <- fromReqURI input :: Maybe Int64
        return $ toSqlKey id

