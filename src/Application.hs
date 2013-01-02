{-# LANGUAGE TemplateHaskell #-}

------------------------------------------------------------------------------
-- | This module defines our application's state type and an alias for its
--   handler monad.
--
module Application where

------------------------------------------------------------------------------
import           Control.Lens
import           Snap.Snaplet
import           Snap.Snaplet.Auth
import           Snap.Snaplet.Heist
import           Snap.Snaplet.I18N
import           Snap.Snaplet.MongoDB.Core
import           Snap.Snaplet.Session

------------------------------------------------------------------------------
data App = App
    { _heist      :: Snaplet (Heist App)
    , _i18n       :: Snaplet I18N
    , _appSession :: Snaplet SessionManager
    , _appMongoDB :: Snaplet MongoDB
    , _appAuth    :: Snaplet (AuthManager App)
    , _adminRole  :: Role                       -- ^ Role for admin user. keep it simple for now.
    }

makeLenses ''App

instance HasHeist App where
    heistLens = subSnaplet heist

instance HasI18N App where
   i18nLens = i18n

instance HasMongoDB App where
    getMongoDB app = app ^. (appMongoDB . snapletValue)
--      getMongoDB = (^& (appMongoDB . snapletValue))

------------------------------------------------------------------------------
type AppHandler = Handler App App
