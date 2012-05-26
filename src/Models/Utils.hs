{-# LANGUAGE OverloadedStrings #-}

{-

utils.

-}

module Models.Utils where

----------------------------------------------------------------

import           Control.Monad.Trans
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text as T
import qualified Data.Text.Encoding as T

----------------------------------------------------------------

-- | Force Just "" to be Nothing.
-- 
forceNonEmpty :: Maybe BS.ByteString -> Maybe BS.ByteString
forceNonEmpty Nothing = Nothing
forceNonEmpty (Just "") = Nothing
forceNonEmpty x = x

----------------------------------------------------------------

textToS :: T.Text -> String
textToS = T.unpack

sToText :: Show s => s -> T.Text
sToText = T.pack . show 

lbsToStrickBS :: LBS.ByteString -> BS.ByteString
lbsToStrickBS = BS.concat . LBS.toChunks

sToBS :: String -> BS.ByteString
sToBS = T.encodeUtf8 . T.pack

bsToS ::  BS.ByteString -> String
bsToS = T.unpack . T.decodeUtf8

lbsToText :: LBS.ByteString -> T.Text
lbsToText = T.decodeUtf8 . lbsToStrickBS

textToBS :: T.Text -> BS.ByteString
textToBS = T.encodeUtf8

loggerDebug :: (Show a, MonadIO m) => a -> m ()
loggerDebug = liftIO . print 


------------------------------------------------------------------------------
