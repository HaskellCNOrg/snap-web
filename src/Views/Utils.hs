{-# LANGUAGE OverloadedStrings #-}

{-

utils.

-}

module Views.Utils where

----------------------------------------------------------------

import           Data.Aeson
import           Control.Applicative
import           Data.Maybe (fromMaybe)
import           Data.Time
import           Snap.Core
import           Snap.Snaplet.Heist
import           System.Locale
import           Text.Digestive
import           Text.Digestive.Heist
import           Text.Templating.Heist
import qualified Data.ByteString as BS
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Data.Text.Read

----------------------------------------------------------------

import           Application
import           Models.Utils
import           Views.DigestiveFunctorSplicesExt

----------------------------------------------------------------
-- Utils for Digestive Functor form

updateViewErrors :: View T.Text -> T.Text -> View T.Text
updateViewErrors v e = v { viewErrors = viewErrors v ++ [([], e)]}

-- | shortcut for render a page with binding DigestiveSplices
-- 
renderDfPage :: BS.ByteString -> View T.Text -> AppHandler ()
renderDfPage p v = renderDfPageSplices p v (bindSplice "dfChildErrorListRef" $ dfChildErrorListRef v)
    --heistLocal ( . bindDigestiveSplices v) $ 
                   --render p

-- | 
-- 
renderDfPageSplices :: BS.ByteString 
                    -> View T.Text 
                    -> (HeistState AppHandler -> HeistState AppHandler)  -- ^ extra splices usually 
                    -> AppHandler ()
renderDfPageSplices p v ss = heistLocal (ss . bindDigestiveSplices v) $ render p

----------------------------------------------------------------

-- | decode parameter which will be "" if not found.
-- 
decodedParam :: MonadSnap m => BS.ByteString -> m BS.ByteString
decodedParam p = fromMaybe "" <$> getParam p

-- | force Just "" to be Nothing during decode.
-- 
decodedParamMaybe :: MonadSnap m => BS.ByteString -> m (Maybe BS.ByteString)
decodedParamMaybe p = forceNonEmpty <$> getParam p

-- | force Just "" to be Nothing during decode.
-- 
decodedParamText :: MonadSnap m => BS.ByteString -> m T.Text
decodedParamText = fmap T.decodeUtf8 . decodedParam

decodedParamTextMaybe :: MonadSnap m => BS.ByteString -> m (Maybe T.Text)
decodedParamTextMaybe p = (fmap T.decodeUtf8 . forceNonEmpty)
                          <$> getParam p

-- | Parse a number
--
decodedParamNum :: (MonadSnap m, Integral a) => BS.ByteString -> m (Maybe a)
decodedParamNum p = (eitherToMaybe . decimal)
                    <$> decodedParamText p
                    where eitherToMaybe = either (const Nothing) (Just . fst)

------------------------------------------------------------------------------

-- | UTCTime to Text
-- 
formatUTCTime :: UTCTime -> T.Text
formatUTCTime = T.pack . formatTime defaultTimeLocale "%F %H:%M"

-- | per Timezone format
formatUTCTimePerTZ :: TimeZone -> UTCTime -> T.Text
formatUTCTimePerTZ tz tm = T.pack . formatTime defaultTimeLocale "%F %H:%M" $ utcToLocalTime tz tm


------------------------------------------------------------------------------
-- JSON Response

contenTypeJSON :: BS.ByteString
contenTypeJSON = "application/json"

setJSONContentType :: Response -> Response
setJSONContentType = setContentType contenTypeJSON

toJSONResponse :: ToJSON a => a -> AppHandler ()
toJSONResponse a = do
  modifyResponse setJSONContentType
  writeLBS . encode . toJSON $ a
