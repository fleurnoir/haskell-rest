module BookStore.Utils where

import Data.Aeson
import Happstack.Server
import qualified Data.Text as T
import Data.Text.Lazy.Encoding
import Control.Monad.IO.Class
import BookStore.EitherTransform
import qualified Data.Text.Encoding as T
import BookStore.ServerAction
import Database.Persist.Sql
import Data.Pool
import Control.Monad.Logger
import Control.Monad
import BookStore.Models
import Data.List
import qualified Data.ByteString.Base64 as B64
import qualified GHC.TypeLits as T

jsonResponse :: ToJSON a => a -> ServerAction Response
jsonResponse object = do
    response <- lift $ ok $ toResponse $ decodeUtf8 $ encode object
    return $ setHeader "Content-Type" "application/json" response

jsonMaybeNotFoundResponse :: ToJSON a => Maybe a -> ServerAction Response
jsonMaybeNotFoundResponse (Just object) = jsonResponse object
jsonMaybeNotFoundResponse Nothing = notFoundResponse "Not found"

notFoundResponse :: T.Text -> ServerAction Response
notFoundResponse = lift . notFound . toResponse

badRequestResponse :: T.Text -> ServerAction Response
badRequestResponse = lift . badRequest . toResponse

unauthorizedResponse :: ServerAction Response
unauthorizedResponse = statusResponse 401 "Unauthorized"

forbiddenResponse :: ServerAction Response
forbiddenResponse = statusResponse 403 "Forbidden"

statusResponse :: Int -> T.Text -> ServerAction Response
statusResponse code = lift . resp code . toResponse

noContentResponse :: ServerAction Response
noContentResponse = lift $ noContent $ toResponse ("" :: T.Text)

getRequestBody :: FromJSON a => ServerAction a
getRequestBody = do
    request <- lift askRq
    maybeBody <- lift $ takeRequestBody request
    case maybeBody of
        Nothing -> throw $ badRequestResponse "Request body required"
        Just body -> case decode $ unBody body of
            Nothing -> throw $ badRequestResponse "Couldn't deserialize response body"
            Just result -> return result

getRequest :: ServerAction Request
getRequest = lift askRq

getRequestHeader :: ServerAction Response -> T.Text -> ServerAction T.Text
getRequestHeader errorResponse key = do
    maybeHeader <- getHeader (T.unpack key) <$> getRequest
    case maybeHeader of
        Nothing -> throw errorResponse
        Just header -> return $ T.decodeUtf8 header

getAuthHeader :: ServerAction T.Text
getAuthHeader = getRequestHeader unauthorizedResponse "Authorization"

runSQL :: Pool SqlBackend -> SqlPersistT (LoggingT IO) a -> IO a
runSQL pool action = runStdoutLoggingT $ runSqlPool action pool