{-# LANGUAGE OverloadedStrings, ExtendedDefaultRules #-}

module Controllers.Tag
       ( routes
       , saveTags
       ) where

import           Snap.Core
import qualified Snap.Core as Snap
import           Snap.Snaplet
import qualified Data.ByteString as BS
import qualified Data.Text as T
import           Data.Bson (ObjectId)

import           Application
import           Models.Tag
import           Views.Utils
import           Controllers.Types

------------------------------------------------------------------------------

-- | Routes
--  /tags -> get all tags
-- 
routes :: [(BS.ByteString, Handler App App ())]
routes =  [ ("/tags",  Snap.method GET getTags)
          ]
  
------------------------------------------------------------------------------

-- | Fetch all tags
-- 
-- FIXME: 1. if content-tye is not json, return empty
-- 
getTags :: AppHandler ()
getTags = findAllTags 
          >>= toJSONResponse

--ids = map textToObjectId ["504181850353211077000002"]

getSomeTags :: [ObjectId] -> AppHandler [Tag]
getSomeTags = findSomeTags

------------------------------------------------------------------------------

-- | Save a list of tags and return them getting ID has been insert.
--   Perform save if is new otherwise ignore.
--
--  FIXME: do not save tags that already exists.
-- 
saveTags :: [T.Text] -> AppHandler [Tag]
saveTags = mapM (insertTag . textToTag)
           where textToTag name = emptyTag { _tagName = name }


----------------------------------------
