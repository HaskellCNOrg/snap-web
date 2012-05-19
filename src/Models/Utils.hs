{-# LANGUAGE OverloadedStrings #-}

{-

utils.

-}

module Models.Utils where

----------------------------------------------------------------

import           Control.Monad.Trans
import           Control.Applicative
import           Data.Maybe (fromMaybe)
import           Snap.Core
import           Snap.Snaplet.Auth
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text.Encoding as T
import qualified Data.Text as T
import qualified Data.Bson as BSON
import Data.Bson
-- import qualified Data.UString as US
-- import qualified Data.ByteString as BS
-- import qualified Data.ByteString.Char8 as B8
-- import qualified Data.CompactString.Internal as CSI
-- import           Numeric (showHex, readHex)
-- import           Safe

----------------------------------------------------------------

forceNonEmpty :: Maybe BS.ByteString -> Maybe BS.ByteString
forceNonEmpty Nothing = Nothing
forceNonEmpty (Just "") = Nothing
forceNonEmpty x = x

----------------------------------------------------------------

stringToText :: Show s => s -> T.Text
stringToText = T.pack . show 

toStrickBS' :: LBS.ByteString -> BS.ByteString
toStrickBS' = BS.concat . LBS.toChunks

pack' :: String -> BS.ByteString
pack' = T.encodeUtf8 . T.pack

unpack' ::  BS.ByteString -> String
unpack' = T.unpack . T.decodeUtf8

lbsToText :: LBS.ByteString -> T.Text
lbsToText = T.decodeUtf8 . toStrickBS'

textToBs :: T.Text -> BS.ByteString
textToBs = T.encodeUtf8

loggerDebug :: (Show a, MonadIO m) => a -> m ()
loggerDebug = liftIO . print 


------------------------------------------------------------------------------
