{-# LANGUAGE TemplateHaskell #-}

------------------------------------------------------------------------------
-- | This module defines our application's state type and an alias for its
--   handler monad.
--
module Application where

------------------------------------------------------------------------------
import Data.Lens.Template
import Data.Time.Clock
import Snap.Snaplet
import Snap.Snaplet.Heist

import Snap.Snaplet.I18N


------------------------------------------------------------------------------
data App = App
    { _heist :: Snaplet (Heist App)
    , _startTime :: UTCTime
    , _i18n   :: Snaplet I18NSnaplet

    }

makeLens ''App

instance HasHeist App where
    heistLens = subSnaplet heist

instance HasI18N App where
   i18nLens = i18n

------------------------------------------------------------------------------
type AppHandler = Handler App App


