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

import           Control.Applicative                         ((<$>))
import           Database.MongoDB                            (host)
import           Snap.Snaplet
import           Snap.Snaplet.Auth
import           Snap.Snaplet.Auth.Backends.MongoDB
import           Snap.Snaplet.Environments
import           Snap.Snaplet.Heist
import           Snap.Snaplet.I18N
import           Snap.Snaplet.MongoDB
import           Snap.Snaplet.Session.Backends.CookieSession

------------------------------------------------------------------------------

import           Application
import           Controllers.Routes
import           Views.SharedSplices
import           Views.Utils

------------------------------------------------------------------------------
-- | The application initializer.

app :: SnapletInit App App
app = makeSnaplet "app" "Happy Haskell, Happy Snap." Nothing $ do
    ul     <- lookupConfig "snaplet.message-locale"
    sk     <- lookupConfigDefault "snaplet.session-key" "data/session-sitekey.txt"
    dbkey  <- lookupConfigDefault "auth.siteKey" "data/auth-sitekey.txt"
    ar     <- Role <$> lookupConfigDefault "auth.admin-role" "administrator"
    dbhost <- lookupConfigDefault "db.host" "127.0.0.1"
    dbc    <- lookupConfigDefault "db.collection" "haskellcn-mongodb"

    h  <- nestSnaplet "heist" heist $ heistInit "templates"
    i  <- nestSnaplet "i18n" i18n $ initI18N ul
    s  <- nestSnaplet "session" appSession $ cookieSessionMgr' sk
    d  <- nestSnaplet "mongoDB" appMongoDB $ mongoDBInit 10 (host dbhost) dbc
    a  <- nestSnaplet "auth" appAuth $ initMongoAuth appSession d (Just dbkey)

    addRoutes routes
    addAuthSplices h appAuth
    addSplices sharedSplices
    return $ App h i s d a ar
  where
    cookieSessionMgr' sk = initCookieSessionManager sk "myapp-session" (Just 600)


------------------------------------------------------------------------------
