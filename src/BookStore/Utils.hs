module BookStore.Utils where

import Data.Aeson
import Happstack.Server
import qualified Data.Text as T
import Data.Text.Lazy.Encoding
import Control.Applicative (Alternative(empty))
import Control.Monad.IO.Class

jsonResponse :: ToJSON a => a -> ServerPartT IO Response
jsonResponse object = do
    response <- ok $ toResponse $ decodeUtf8 $ encode object
    return $ setHeader "Content-Type" "application/json" response

jsonMaybeResponse :: ToJSON a => Maybe a -> ServerPartT IO Response
jsonMaybeResponse (Just object) = jsonResponse object
jsonMaybeResponse Nothing = notFoundResponse

notFoundResponse :: ServerPartT IO Response
notFoundResponse = notFound $ toResponse ("Nothing was Found" :: T.Text)

badRequestResponse :: ServerPartT IO Response
badRequestResponse = notFound $ toResponse ("Nothing was Found" :: T.Text)

getRequestBody :: FromJSON a => ServerPartT IO (Maybe a)
getRequestBody = do
    request <- askRq
    maybeBody <- liftIO $ takeRequestBody request
    return $ do 
        body <- maybeBody
        decode $ unBody body
