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

import           Control.Monad 
import           Control.Monad.Trans
import           Control.Monad.Reader
import           Database.MongoDB (host)
import           Data.Maybe (fromMaybe)
import           Snap.Snaplet
import           Snap.Snaplet.Auth
import           Snap.Snaplet.Auth.Backends.MongoDB
import           Snap.Snaplet.Heist
import           Snap.Snaplet.I18N
import           Snap.Snaplet.MongoDB
import           Snap.Snaplet.Session.Backends.CookieSession
import qualified Data.Configurator as C
import qualified Data.Configurator.Types as C
import qualified Data.UString as US

------------------------------------------------------------------------------

import           Application
import           Controllers.Routes
import           Views.SharedSplices

------------------------------------------------------------------------------
-- | The application initializer.

app :: SnapletInit App App
app = makeSnaplet "a.haskellcn" "Happy Haskell, Happy Snap." Nothing $ do
    config <- getSnapletUserConfig
    ul     <- lookupSnapletUserConfig "snaplet.message-locale"
    sk     <- lookupSnapletUserConfigDe "snaplet.session-key" "data/session-sitekey.txt"
    dbhost <- lookupSnapletUserConfigDe "db.host" "127.0.0.1"
    dbc    <- lookupSnapletUserConfigDe "db.collection" "haskellcn-mongodb"  
    dbkey  <- lookupSnapletUserConfigDe "db.siteKey" "data/auth-sitekey.txt" 

    h  <- nestSnaplet "heist" heist $ heistInit "templates"
    i  <- nestSnaplet "i18n" i18n $ initI18NSnaplet ul
    s  <- nestSnaplet "session" appSession $ cookieSessionMgr' sk
    d  <- nestSnaplet "mongoDB" appMongoDB $ mongoDBInit 10 (host dbhost) (US.u dbc)
    a  <- nestSnaplet "auth" appAuth $ initMongoAuth appSession d (Just dbkey)
    addRoutes routes
    addAuthSplices appAuth
    addSplices sharedSplices
    return $ App h i s d a
  where
    cookieSessionMgr' sk = initCookieSessionManager sk "myapp-session" (Just 600)


------------------------------------------------------------------------------

lookupSnapletUserConfig :: (MonadIO (m b v), MonadSnaplet m, C.Configured a) => C.Name -> m b v (Maybe a)
lookupSnapletUserConfig name = do
    config <- getSnapletUserConfig
    liftIO $ C.lookup config name

lookupSnapletUserConfigDe :: (MonadIO (m b v), MonadSnaplet m, C.Configured a) 
                          => C.Name  -- ^ Key
                          -> a       -- ^ default value
                          -> m b v a
lookupSnapletUserConfigDe name def = liftM (fromMaybe def) (lookupSnapletUserConfig name)



--a  <- nestSnaplet "auth" appAuth $ initJsonFileAuthManager authSettings' appSession "data/auth.json"
--     authSettings'    = defAuthSettings { asSiteKey = "data/auth-sitekey.txt" }
