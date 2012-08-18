{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}

module Snap.Snaplet.Environments
    ( module Data.Configurator
    , lookupConfig
    , lookupConfigDefault
    ) 
    where

import           Control.Monad.Reader
import           Data.Configurator
import           Data.Configurator.Types
import           Data.List (find)
import           Data.Maybe (fromMaybe)
import           Snap.Snaplet
import qualified Data.HashMap.Lazy                   as HM
import qualified Data.Text                           as T
import qualified Data.Text as T
import qualified Data.UString as U


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

-----------------------------------------------------------

-- | This is required for `mongoDBInit` from Snaplet.MongoDB
-- 
instance Configured U.UString where
  convert (String t) = Just $ U.u $ T.unpack t
  convert _ = Nothing
