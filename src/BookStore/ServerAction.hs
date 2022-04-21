{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module BookStore.ServerAction where
    
import Database.Persist.Sql
import Data.Pool
import Happstack.Server
import Control.Monad.Reader
import Control.Monad.Logger
import BookStore.Repository.Book
import BookStore.Repository.User

newtype ServerEnv = ServerEnv {sqlPool :: Pool SqlBackend}

type ServerAction = ServerPartT (ReaderT ServerEnv IO)

instance BookRepositoryProvider ServerAction (SqlPersistT (LoggingT IO)) where
    runBooksRepo = runDB

instance UserRepositoryProvider ServerAction (SqlPersistT (LoggingT IO)) where
    runUsersRepo = runDB

runDB :: SqlPersistT (LoggingT IO) a -> ServerAction a
runDB dbAction = do
    pool <- asks sqlPool
    liftIO $ runStdoutLoggingT $ runSqlPool dbAction pool