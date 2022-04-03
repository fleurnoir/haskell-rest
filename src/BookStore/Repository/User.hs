module BookStore.Repository.User where
import Control.Monad.IO.Class
import qualified Data.Text as T
import Database.Persist.Sql
import BookStore.Models

getUser :: MonadIO m => T.Text -> SqlPersistT m (Maybe User)
getUser "John" = return $ Just (User "John" "12345" "Admin,User")
getUser "Bob" = return $ Just (User "Bob" "12345" "User")
getUser _ = return Nothing