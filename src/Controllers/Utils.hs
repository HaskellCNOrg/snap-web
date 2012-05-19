{-# LANGUAGE OverloadedStrings #-}

{-

ALL utils just sit here. Maybe separation.

-}

module Controllers.Utils where

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

----------------------------------------------------------------

import           Text.Digestive
import           Text.Digestive.Heist
import           Snap.Snaplet.Heist
import           Application
import           Models.Utils

----------------------------------------------------------------
-- Utils for Digestive Functor form

updateViewErrors :: View T.Text -> T.Text -> View T.Text
updateViewErrors v e = v { viewErrors = viewErrors v ++ [([], e)]}


renderDfPage :: BS.ByteString -> View T.Text -> AppHandler ()
renderDfPage p v = heistLocal (bindDigestiveSplices v) $ render p

----------------------------------------------------------------

decodedParam :: MonadSnap m => BS.ByteString -> m BS.ByteString
decodedParam p = fromMaybe "" <$> getParam p

-- | force Just "" to be Nothing during decode.
-- 
decodedParam' :: MonadSnap m => BS.ByteString -> m (Maybe BS.ByteString)
decodedParam' p = forceNonEmpty <$> getParam p

decodedParamText :: MonadSnap m => BS.ByteString -> m (Maybe T.Text)
decodedParamText p = fmap T.decodeUtf8 <$> forceNonEmpty <$>getParam p

----------------------------------------------------------------

