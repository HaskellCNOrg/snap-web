{-# LANGUAGE OverloadedStrings #-}

module Controllers.Types where

import Data.Aeson
import Models.Tag
import Models.Utils
import Data.Bson (ObjectId)

instance ToJSON Tag where
  toJSON (Tag id name _) = object [ "id"   .= id
                                  , "name" .= name
                                  ]
instance ToJSON ObjectId where
  toJSON = String . sToText
