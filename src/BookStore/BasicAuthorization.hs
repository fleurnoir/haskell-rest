module BookStore.BasicAuthorization(
    authorize
) where

import Data.Pool
import Database.Persist.Sql
import qualified Data.Text as T
import BookStore.ServerAction
import qualified Data.ByteString.Base64 as B64
import qualified Data.Text.Encoding as T
import BookStore.Utils
import BookStore.Repository.User
import BookStore.Models
import Data.List
import Control.Monad
import Control.Monad.IO.Class

authorize :: (T.Text -> IO (Maybe User)) -> [T.Text] -> ServerAction ()
authorize getUser allowedRoles = do
    (username, password) <- getCredentials
    maybeUser <- liftIO $ getUser username
    case maybeUser of
        Nothing -> throw unauthorizedResponse
        Just user ->
            let actualRoles = T.splitOn "," (userRoles user)
                acceptedRoles = intersect actualRoles allowedRoles
            in when (userPassword user /= password || null acceptedRoles) $ throw forbiddenResponse

getCredentials :: ServerAction (T.Text, T.Text)
getCredentials = do
    header <- getAuthHeader
    let authPrefix = "Basic "
    if not $ T.isPrefixOf authPrefix header
        then throw unauthorizedResponse
        else 
            -- cutting of the "Basic " prefix and decoding credentials from Base64
            let eitherDecodedCredentials = B64.decode $ T.encodeUtf8 $ T.drop (T.length authPrefix) header
            in case eitherDecodedCredentials of
                Left _ -> throw unauthorizedResponse
                Right decodedCredentials -> 
                    let credentials = T.splitOn ":" $ T.decodeUtf8 decodedCredentials
                    in case credentials of
                        [username, password] -> return (username, password)
                        _ -> throw unauthorizedResponse