{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE OverloadedStrings    #-}

module Controllers.Exception where

import           Data.ByteString    (ByteString)
import qualified Data.ByteString    as BS
import           Data.Text          (Text)
import qualified Heist.Interpreted  as I
import           Snap.Core
import           Snap.Snaplet
import           Snap.Snaplet.Heist

import           Application
import           Models.Exception
import           Models.Utils

------------------------------------------------------------------------------

routes :: [(ByteString, Handler App App ())]
routes = [ ("",             fourofourH)
         ]

fourofourH :: AppHandler ()
fourofourH = do
  -- FIXME: this has been print twice. why??
  -- liftIO $ print "error handler"
  modifyResponse (setResponseStatus 404 "Not Found")
  req <- getRequest
  toErrorPage . bsToText $ "No handler accepted " `BS.append` rqURI req
  r <- getResponse
  finishWith r


exceptionH :: UserException -> AppHandler ()
exceptionH = toErrorPage . showUE


toErrorPage :: Text           -- ^ Errors
               -> AppHandler ()
toErrorPage err = heistLocal (I.bindSplice "error" (I.textSplice err)) $ render "error-page"
