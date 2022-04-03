{-# LANGUAGE UndecidableInstances #-}

module Main where

import BookStore.Models
import Database.Persist.Sql
import Control.Monad.Logger
import Database.Persist.Sqlite
import Happstack.Server
import Control.Monad
import Control.Monad.Trans.Class
import Data.Pool
import BookStore.Repository.Book
import BookStore.Repository.User
import Data.Int
import Happstack.Server.Internal.Monads
import qualified Data.Text as T
import BookStore.EitherTransform
import BookStore.ServerAction
import BookStore.Utils
import qualified BookStore.BasicAuthorization as Basic
import Control.Monad.IO.Class

main = do
    pool <- runStdoutLoggingT $ createSqlitePool "api.db" 5
    runSqlPool (runMigration migrateAll) pool
    simpleHTTP nullConf $ server pool

server :: Pool SqlBackend -> ServerPartT IO Response
server pool = msum [
        dir "book" $ path $ \id -> do
            method GET
            runServerAction $ do
                authorize ["User"]
                runSQLAction (getBook id) >>= jsonMaybeNotFoundResponse,
        dir "book" $ do
            method GET
            runServerAction $ do
                authorize ["User"]
                runSQLAction getBooks >>= jsonResponse,
        dir "book" $ do
            method POST
            runServerAction $ do
                authorize ["Admin"]
                book <- getRequestBody :: ServerAction Book
                runSQLAction (insertBook book) >>= jsonResponse,
        dir "book" $ do
            method PUT
            runServerAction $ do
                authorize ["Admin"]
                book <- getRequestBody :: ServerAction (Entity Book)
                runSQLAction (updateBook book)
                noContentResponse,
        dir "book" $ path $ \id -> do
            method DELETE
            runServerAction $ do
                authorize ["Admin"]
                runSQLAction (deleteBook id)
                noContentResponse
    ]
    where 
        authorize = Basic.authorize (runSQL pool . getUser)
        runSQLAction = lift . liftIO . runSQL pool

instance (ToBackendKey SqlBackend a) => FromReqURI (Key a) where
    fromReqURI input = do
        id <- fromReqURI input :: Maybe Int64
        return $ toSqlKey id


