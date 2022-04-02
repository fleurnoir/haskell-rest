module BookStore.Repository.User where
import Control.Monad.IO.Class
import qualified Data.Text as T
import Database.Persist.Sql
import BookStore.Models

getUser :: MonadIO m => T.Text -> SqlPersistT m (Maybe User)
getUser "admin" = return $ Just (User "admin" "12345" "Admin,User")
getUser "user" = return $ Just (User "user" "12345" "User")
getUser _ = return Nothing