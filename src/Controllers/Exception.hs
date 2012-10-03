{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE OverloadedStrings    #-}

module Controllers.Exception where

import           Control.Monad.Trans
import           Data.ByteString       (ByteString)
import qualified Data.ByteString       as BS
import           Data.Text             (Text)
import           Snap.Core
import           Snap.Snaplet
import           Snap.Snaplet.Heist
import           Text.Templating.Heist

import           Application
import           Models.Exception
import           Models.Utils
import           Views.Utils

------------------------------------------------------------------------------

routes :: [(ByteString, Handler App App ())]
routes = [ ("",             fourofourH)
         ]

fourofourH :: AppHandler ()
fourofourH = do
  modifyResponse (setResponseStatus 404 "Not Found")
  req <- getRequest
  toErrorPage . bsToText $ "No handler accepted " `BS.append` rqURI req


exceptionH :: UserException -> AppHandler ()
exceptionH = toErrorPage . showUE


toErrorPage :: Text           -- ^ Errors
               -> AppHandler ()
toErrorPage err = heistLocal (bindSplice "error" (textSplice err)) $ render "error-page"
