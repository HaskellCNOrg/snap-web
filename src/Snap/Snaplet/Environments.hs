{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Snap.Snaplet.Environments
    ( module Data.Configurator
    , lookupConfig
    , lookupConfigDefault
    )
    where

import           Control.Monad.Reader
import           Data.Configurator
import           Data.Configurator.Types
import           Data.Maybe              (fromMaybe)
import           Snap.Snaplet


-----------------------------------------------------------
--

-- | Lookup for a config value
--
lookupConfig :: (MonadIO (m b v), MonadSnaplet m, Configured a) => Name -> m b v (Maybe a)
lookupConfig name = do
    config <- getSnapletUserConfig
    liftIO $ Data.Configurator.lookup config name

-- | Lookup for a config value.
--   Return default value otherwise.
--
lookupConfigDefault :: (MonadIO (m b v), MonadSnaplet m, Configured a)
                          => Name    -- ^ Key
                          -> a       -- ^ default value
                          -> m b v a
lookupConfigDefault name def = liftM (fromMaybe def) (lookupConfig name)
