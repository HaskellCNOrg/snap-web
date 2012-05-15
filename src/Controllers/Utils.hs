{-# LANGUAGE OverloadedStrings #-}

module Controllers.Utils where

import           Control.Applicative
import           Data.Maybe (fromMaybe)
import           Snap.Core
import           Data.ByteString

decodedParam :: MonadSnap m => ByteString -> m ByteString
decodedParam p = fromMaybe "" <$> getParam p
