{-# LANGUAGE CPP               #-}
{-# LANGUAGE OverloadedStrings #-}

{-

utils.

-}

module Views.Utils where

----------------------------------------------------------------

import           Control.Applicative
import           Data.Aeson
import qualified Data.ByteString         as BS
import           Data.CaseInsensitive    (CI)
import           Data.Maybe              (fromMaybe)
import           Data.Monoid             (mconcat)
import qualified Data.Text               as T
import qualified Data.Text.Encoding      as T
import           Data.Text.Read
import           Data.Time
import           Data.Time.Relative
import           Heist
import qualified Heist.Interpreted       as I
import           Prelude                 hiding (lookup)
import           Snap
import           Snap.Core
import           Snap.Snaplet.Heist
import           Snap.Types.Headers      (lookup)
import           System.Locale
import           Text.Digestive
import           Text.Digestive.Heist
----------------------------------------------------------------
import           Application

import           Models.Utils
import           Text.Digestive.HeistExt

----------------------------------------------------------------
-- Utils for Digestive Functor form

updateViewErrors :: View T.Text -> T.Text -> View T.Text
updateViewErrors v e = v { viewErrors = viewErrors v ++ [([], e)]}

-- | shortcut for render a page with binding DigestiveSplices
--
renderDfPage :: BS.ByteString -> View T.Text -> AppHandler ()
renderDfPage p v = renderDfPageSplices p v (I.bindSplice "dfChildErrorListRef" $ dfChildErrorListRef v)
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

-- | per Timezone format
formatUTCTimePerTZ :: TimeZone -> UTCTime -> T.Text
formatUTCTimePerTZ tz tm = T.pack . formatTime defaultTimeLocale "%F %H:%M" $ utcToLocalTime tz tm

formatUTCTimeChina :: UTCTime -> T.Text
formatUTCTimeChina = formatUTCTimePerTZ timezoneChina

timezoneChina :: TimeZone
timezoneChina = TimeZone 480 False "CST"

-- | UTCTime to Text as Beijing time.
-- TODO: cpp compile doesnt work, why??
--       #if TIMEZONE==CST
--       formatUTCTime = T.pack . formatTime defaultTimeLocale "%F %H:%M"
--
formatUTCTime :: UTCTime -> T.Text
formatUTCTime = formatUTCTimeChina

relativeUTCTime :: UTCTime -> UTCTime -> T.Text
relativeUTCTime t1 t2 = T.pack $ relative t1 t2 True

------------------------------------------------------------------------------
-- JSON Response

contentTypeJSON :: BS.ByteString
contentTypeJSON = "application/json"

setJSONContentType :: Response -> Response
setJSONContentType = setContentType contentTypeJSON

toJSONResponse :: ToJSON a => a -> AppHandler ()
toJSONResponse a = do
  modifyResponse setJSONContentType
  writeLBS . encode . toJSON $ a

------------------------------------------------------------------------------
-- Headers
acceptHeaderName :: CI BS.ByteString
acceptHeaderName = "Accept"

hasAcceptHeaderJSON :: Headers -> Bool
hasAcceptHeaderJSON xs =
  case lookup acceptHeaderName xs of
    Just (x:_) -> contentTypeJSON `BS.isPrefixOf` x
    _ -> False


------------------------------------------------------------------------------
-- Splice

-- | Fast migrate for heist 0.13 change
--
foldSplices :: [(T.Text, I.Splice AppHandler)]
               -> Splices (I.Splice AppHandler)
foldSplices = mconcat . map (uncurry (##))

addSplices :: [(T.Text, I.Splice AppHandler)]
              -> Initializer App v ()
addSplices = modifyHeistState . I.bindSplices . foldSplices

subTitleSplice :: Maybe T.Text -> Splices (I.Splice AppHandler)
subTitleSplice maybeVal = let val = maybe "" (T.append " :: ") maybeVal
	       		  in
	       		   "subTitle" ## I.textSplice val

