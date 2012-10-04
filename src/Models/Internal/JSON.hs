{-# LANGUAGE OverloadedStrings #-}

module Models.Internal.JSON where

import           Data.Aeson (ToJSON(..), Value(String))
import           Data.Bson    (ObjectId)
import           Models.Utils

instance ToJSON ObjectId where
  toJSON = String . sToText
