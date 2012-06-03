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

import           Database.MongoDB (host)
import           Snap.Snaplet
import           Snap.Snaplet.Auth
import           Snap.Snaplet.Auth.Backends.MongoDB
import           Snap.Snaplet.Heist
import           Snap.Snaplet.I18N
import           Snap.Snaplet.MongoDB
import           Snap.Snaplet.Session.Backends.CookieSession

------------------------------------------------------------------------------

import           Application
import           Controllers.Routes
import           Views.SharedSplices

------------------------------------------------------------------------------
-- | The application initializer.

app :: SnapletInit App App
app = makeSnaplet "app" "An snaplet example application." Nothing $ do
    h  <- nestSnaplet "heist" heist $ heistInit "templates"
    i  <- nestSnaplet "i18n" i18n $ initI18NSnaplet (Just "zh_CN")
    s  <- nestSnaplet "session" appSession cookieSessionMgr'
    d  <- nestSnaplet "mongoDB" appMongoDB $ mongoDBInit 10 (host "127.0.0.1") "haskellcn-mongodb"    
    a  <- nestSnaplet "auth" appAuth $ initMongoAuth appSession d (Just "data/auth-sitekey.txt")
    addRoutes routes
    addAuthSplices appAuth
    addSplices sharedSplices
    return $ App h i s d a
  where
    cookieSessionMgr' = initCookieSessionManager "data/session-sitekey.txt" "myapp-session" (Just 600)


------------------------------------------------------------------------------







--a  <- nestSnaplet "auth" appAuth $ initJsonFileAuthManager authSettings' appSession "data/auth.json"
--     authSettings'    = defAuthSettings { asSiteKey = "data/auth-sitekey.txt" }
