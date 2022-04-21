{-# LANGUAGE FlexibleContexts #-}
module BookStore.Utils where

import Data.Aeson
import Happstack.Server
import qualified Data.Text as T
import Data.Text.Lazy.Encoding
import Control.Monad.IO.Class
import qualified Data.Text.Encoding as T

jsonResponse :: Monad m => ToJSON a => a -> ServerPartT m Response
jsonResponse object = do
    response <- ok $ toResponse $ decodeUtf8 $ encode object
    return $ setHeader "Content-Type" "application/json" response

jsonMaybeNotFoundResponse :: Monad m => ToJSON a => Maybe a -> ServerPartT m Response
jsonMaybeNotFoundResponse (Just object) = jsonResponse object
jsonMaybeNotFoundResponse Nothing = return $ notFoundResponse "Not found"

notFoundResponse :: String -> Response
notFoundResponse = result 404

badRequestResponse :: String -> Response
badRequestResponse = result 400

unauthorizedResponse :: Response
unauthorizedResponse = result 401 "Unauthorized"

forbiddenResponse :: Response
forbiddenResponse = result 403 "Forbidden"

noContentResponse :: Response
noContentResponse = result 204 "No Content"

getRequestBody :: MonadIO m => FromJSON a => ServerPartT m a
getRequestBody = do
    request <- askRq
    maybeBody <- takeRequestBody request
    case maybeBody of
        Nothing -> finishWith $ badRequestResponse "Request body required"
        Just body -> case decode $ unBody body of
            Nothing -> finishWith $ badRequestResponse "Couldn't deserialize response body"
            Just result -> return result

getRequestHeader :: (WebMonad Response m, ServerMonad m) => Response -> T.Text -> m T.Text
getRequestHeader errorResponse key = do
    maybeHeader <- getHeader (T.unpack key) <$> askRq
    case maybeHeader of
        Nothing -> finishWith errorResponse
        Just header -> return $ T.decodeUtf8 header

getAuthHeader :: (WebMonad Response m, ServerMonad m) => m T.Text
getAuthHeader = getRequestHeader unauthorizedResponse "Authorization"
