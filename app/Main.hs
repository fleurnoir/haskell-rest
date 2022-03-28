{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE TypeFamilies               #-}

module Main where
import Data.Text as T
import Web.Spock
import Web.Spock.Config
import Control.Monad.IO.Class
import BookStore.Models
import qualified Database.Persist as P
import Database.Persist.Sqlite hiding (get)
import Control.Monad.Logger (LoggingT, runStdoutLoggingT)
import Data.Aeson hiding (json)
import Data.Aeson.Types
import Network.HTTP.Simple (getRequestHeader)

runSQL :: (HasSpock m, SpockConn m ~ SqlBackend) => SqlPersistT (LoggingT IO) a -> m a
runSQL action = runQuery $ \conn -> runStdoutLoggingT $ runSqlConn action conn

errorJson :: Int -> Text -> ApiAction ()
errorJson code message =
  json $
    object [ 
        "result" .= String "failure", 
        "error" .= object [
            "code" .= code, 
            "message" .= message
        ]
    ]

main :: IO ()
main = do
    pool <- runStdoutLoggingT $ createSqlitePool "api.db" 5
    spockCfg <- defaultSpockCfg () (PCPool pool) ()
    runStdoutLoggingT $ runSqlPool (do runMigration migrateAll) pool
    runSpock 8080 (spock spockCfg app)

type Api = SpockM SqlBackend () () ()

type ApiAction a = SpockAction SqlBackend () () a

app :: Api
app = do 
    get "book" $ do
        sess <- getState  
        allBooks <- runSQL $ selectList [] [Asc BookId]
        json allBooks
    
    get ("book" <//> var) $ \bookId -> do
        maybeBook <- runSQL $ P.get bookId :: ApiAction (Maybe Book)
        case maybeBook of
            Nothing -> errorJson 2 "Could not find a book with matching id"
            Just theBook -> json theBook

    post "book" $ do
        maybeBook <- jsonBody' :: ApiAction (Maybe Book)
        case maybeBook of
            Nothing -> errorJson 1 "Failed to parse request body as Book"
            Just theBook -> do
                newId <- runSQL $ insert theBook
                json $ object ["result" .= String "success", "id" .= newId]

    put ("book" <//> var) $ \bookId -> do 
        maybeBook <- jsonBody' :: ApiAction (Maybe Book)
        case maybeBook of
            Nothing -> errorJson 1 "Failed to parse request body as Book"
            Just theBook -> do
                runSQL $ update (bookId :: Key Book) [
                    BookAuthor =. bookAuthor theBook, 
                    BookTitle =. bookTitle theBook,
                    BookYear =. bookYear theBook]
                json $ object ["result" .= String "success", "id" .= bookId]