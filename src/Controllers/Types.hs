{-# LANGUAGE OverloadedStrings #-}

module Controllers.Types where

import           Data.Aeson
import           Data.Bson    (ObjectId)
import           Models.Tag
import           Models.Utils

instance ToJSON Tag where
  toJSON (Tag id name _) = object [ "id"   .= id
                                  , "name" .= name
                                  ]
instance ToJSON ObjectId where
  toJSON = String . sToText
