{-# LANGUAGE OverloadedStrings #-}

------------------------------------------------------------------------------
-- | This module is where all the routes and handlers are defined for your
--   site. The 'app' function is the initializer that combines everything
--   together and is exported by this module.
--
module Controllers.Site
  ( app
  ) where

------------------------------------------------------------------------------
import           Control.Applicative
import           Control.Monad.State
import           Control.Monad.Trans
import           Data.ByteString (ByteString)
import           Data.Maybe
import           Data.Time.Clock
import           Snap.Core
import           Snap.Snaplet
import           Snap.Snaplet.Auth
import           Snap.Snaplet.Auth.Backends.JsonFile
import           Snap.Snaplet.Heist
import           Snap.Snaplet.I18N
import           Snap.Snaplet.Session.Backends.CookieSession
import           Snap.Util.FileServe
import           Text.Templating.Heist
import           Text.XmlHtml hiding (render)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
------------------------------------------------------------------------------

import           Application
import           Controllers.Routes

------------------------------------------------------------------------------
-- | The application initializer.
app :: SnapletInit App App
app = makeSnaplet "app" "An snaplet example application." Nothing $ do
    h  <- nestSnaplet "heist" heist $ heistInit "templates"
    i  <- nestSnaplet "i18n" i18n $ initI18NSnaplet (Just "zh_CN") Nothing
    s  <- nestSnaplet "session" appSession cookieSessionMgr'  
    a  <- nestSnaplet "auth" appAuth $ initJsonFileAuthManager authSettings' appSession "log/auth.json"
    addRoutes routes
    addAuthSplices appAuth    
    return $ App h i s a
  where
    cookieSessionMgr' = initCookieSessionManager "log/cookies-sitekey.txt" "myapp-session" (Just 600)
    authSettings'    = defAuthSettings { asSiteKey = "log/auth-sitekey.txt" }


------------------------------------------------------------------------------

