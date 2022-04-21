{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module BookStore.Repository.Book where

import Control.Monad.IO.Class
import Database.Persist.Sql
import BookStore.Models

class MonadIO m => BookRepository m where
    getBooks :: m [Entity Book]
    getBook :: Key Book -> m (Maybe (Entity Book))
    insertBook :: Book -> m (Key Book)
    updateBook :: Entity Book -> m ()
    deleteBook :: Key Book -> m ()

class (BookRepository m) => BookRepositoryProvider p m where
    runBooksRepo :: m a -> p a  

instance (MonadIO m) => BookRepository (SqlPersistT m) where
    getBooks :: MonadIO m => SqlPersistT m [Entity Book]
    getBooks = selectList [] [Asc BookId]

    getBook :: MonadIO m => Key Book -> SqlPersistT m (Maybe (Entity Book))
    getBook key = do
        maybeBook <- get key
        return $ Entity key <$> maybeBook

    insertBook :: MonadIO m => Book -> SqlPersistT m (Key Book)
    insertBook = insert

    updateBook :: MonadIO m => Entity Book -> SqlPersistT m ()
    updateBook book =
        let key = entityKey book
            value = entityVal book
        in update key [
                BookAuthor =. bookAuthor value,
                BookTitle =. bookTitle value,
                BookYear =. bookYear value
            ]

    deleteBook :: MonadIO m => Key Book -> SqlPersistT m ()
    deleteBook = delete


