{-# LANGUAGE OverloadedStrings #-}

module Controllers.Utils where

import           Control.Monad.Trans
import           Control.Applicative
import           Data.Maybe (fromMaybe)
import           Snap.Core
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text.Encoding as T
import qualified Data.Text as T

decodedParam :: MonadSnap m => BS.ByteString -> m BS.ByteString
decodedParam p = fromMaybe "" <$> getParam p

toStrickBS' :: LBS.ByteString -> BS.ByteString
toStrickBS' = BS.concat . LBS.toChunks

pack' :: String -> BS.ByteString
pack' = T.encodeUtf8 . T.pack

lbsToText :: LBS.ByteString -> T.Text
lbsToText = T.decodeUtf8 . toStrickBS'

textToBs :: T.Text -> BS.ByteString
textToBs = T.encodeUtf8

loggerDebug :: (Show a, MonadIO m) => a -> m ()
loggerDebug = liftIO . print 
