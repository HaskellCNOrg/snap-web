{-# LANGUAGE OverloadedStrings #-}

{-

utils.

-}

module Models.Utils where

----------------------------------------------------------------

import           Control.Monad.Trans
import           Data.Bson            (ObjectId, Value (..))
import qualified Data.Bson            as BSON
import qualified Data.ByteString      as BS
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text            as T
import qualified Data.Text.Encoding   as T

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

-- | Transform something which is instance of <code>Show</code> to Text.
--
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

bsToText :: BS.ByteString -> T.Text
bsToText = T.decodeUtf8

------------------------------------------------------------------------------

-- | NOTE: exception "no parse" if failed to convert.
--
textToObjectId :: T.Text -> ObjectId
textToObjectId = read . textToS

-- | Maybe ObjectId to Text
--
objectIdToText :: Maybe ObjectId -> T.Text
objectIdToText = maybe "" sToText

-- | Case MongoDB.value to ObjectId
--
objectIdFromValue :: Value -> Maybe ObjectId
objectIdFromValue = BSON.cast'

------------------------------------------------------------------------------

-- | Split Text by space or comma and get ride of extra empty text.
--
splitOnSpaceOrComma :: T.Text -> [T.Text]
splitOnSpaceOrComma = filter (/= T.pack "") . T.split (\x -> x == ',' || x == ' ')
