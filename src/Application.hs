{-# LANGUAGE TemplateHaskell #-}

------------------------------------------------------------------------------
-- | This module defines our application's state type and an alias for its
--   handler monad.
--
module Application where

------------------------------------------------------------------------------
import Data.Lens.Template
import Snap.Snaplet
import Snap.Snaplet.Heist
import Snap.Snaplet.I18N
import Snap.Snaplet.Auth
--import Snap.Snaplet.MongoDB
import Snap.Snaplet.Session

------------------------------------------------------------------------------
data App = App
    { _heist      :: Snaplet (Heist App)
    , _i18n       :: Snaplet I18NSnaplet
    , _appSession :: Snaplet SessionManager
    , _appAuth    :: Snaplet (AuthManager App)
    }

makeLens ''App

instance HasHeist App where
    heistLens = subSnaplet heist

instance HasI18N App where
   i18nLens = i18n

------------------------------------------------------------------------------
type AppHandler = Handler App App

