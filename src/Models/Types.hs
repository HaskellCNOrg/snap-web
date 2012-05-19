{-# LANGUAGE OverloadedStrings #-}


module Models.Types where

----------------------------------------------------------------

import Data.Bson
import Data.Baeson.Types
import Control.Applicative

----------------------------------------------------------------

instance FromBSON ObjectId where
  fromBSON (ObjId x) = pure x


instance ToBSON ObjectId where
  toBSON = ObjId
