module BookStore.Actions where

import Happstack.Server
import Database.Persist.Sql
import Control.Monad.Logger
import BookStore.Models
import qualified Data.Text as T
import qualified BookStore.BasicAuthorization as Basic
import Control.Monad.Trans.Class
import BookStore.Repository.Book
import BookStore.Utils
import BookStore.Repository.User
import Control.Applicative
import Control.Monad
import BookStore.ServerAction

getBookAction :: Key Book -> ServerAction Response
getBookAction id = do
    method GET
    authorize ["User"]
    runDB (getBook id) >>= jsonMaybeNotFoundResponse

getBooksAction :: ServerAction Response
getBooksAction = do
    method GET
    authorize ["User"]
    runDB getBooks >>= jsonResponse

insertBookAction :: ServerAction Response
insertBookAction = do
    method POST
    authorize ["Admin"]
    book <- getRequestBody :: ServerAction Book
    runDB (insertBook book) >>= jsonResponse

updateBookAction :: ServerAction Response
updateBookAction = do
    method PUT
    authorize ["Admin"]
    book <- getRequestBody :: ServerAction (Entity Book)
    runDB $ updateBook book
    return noContentResponse

deleteBookAction :: Key Book -> ServerAction Response
deleteBookAction id = do
    method DELETE
    authorize ["Admin"]
    runDB (deleteBook id)
    return noContentResponse

authorize :: [T.Text] -> ServerAction ()
authorize = Basic.authorize (runDB . getUser)

instance (Monad m, Alternative m) => Alternative (LoggingT m) where
    empty = lift empty
    x <|> y = LoggingT $ \logger -> runLoggingT x logger <|> runLoggingT y logger

instance (MonadPlus m) => MonadPlus (LoggingT m)

