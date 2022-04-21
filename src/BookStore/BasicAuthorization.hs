{-# LANGUAGE FlexibleContexts #-}
module BookStore.BasicAuthorization(
    authorize
) where

import qualified Data.Text as T
import qualified Data.ByteString.Base64 as B64
import qualified Data.Text.Encoding as T
import BookStore.Utils
import BookStore.Models
import Data.List
import Control.Monad
import Happstack.Server

authorize :: (WebMonad Response m, ServerMonad m) => (T.Text -> m (Maybe User)) -> [T.Text] -> m ()
authorize getUser allowedRoles = do
    (username, password) <- getCredentials
    maybeUser <- getUser username
    case maybeUser of
        Nothing -> finishWith unauthorizedResponse
        Just user ->
            let actualRoles = T.splitOn "," (userRoles user)
                acceptedRoles = intersect actualRoles allowedRoles
            in when (userPassword user /= password || null acceptedRoles) $ finishWith forbiddenResponse

getCredentials :: (WebMonad Response m, ServerMonad m) => m (T.Text, T.Text)
getCredentials = do
    header <- getAuthHeader
    let authPrefix = "Basic "
    if not $ T.isPrefixOf authPrefix header
        then finishWith unauthorizedResponse
        else 
            -- cutting of the "Basic " prefix and decoding credentials from Base64
            let eitherDecodedCredentials = B64.decode $ T.encodeUtf8 $ T.drop (T.length authPrefix) header
            in case eitherDecodedCredentials of
                Left _ -> finishWith unauthorizedResponse
                Right decodedCredentials -> 
                    let credentials = T.splitOn ":" $ T.decodeUtf8 decodedCredentials
                    in case credentials of
                        [username, password] -> return (username, password)
                        _ -> finishWith unauthorizedResponse

