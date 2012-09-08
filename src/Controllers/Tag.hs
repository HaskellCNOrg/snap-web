{-# LANGUAGE OverloadedStrings, ExtendedDefaultRules #-}

module Controllers.Tag
       ( routes
       , saveTags
       , filterExistsTags
       ) where

import           Snap.Core
import qualified Snap.Core as Snap
import           Snap.Snaplet
import qualified Data.ByteString as BS
import qualified Data.Text as T
import           Data.Bson (ObjectId)
import           Data.List (deleteFirstsBy)

import           Application
import           Models.Tag
import           Views.Utils
import           Controllers.Types
import Control.Monad.Trans

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
-- MAYBE: 1. if content-tye is not json, return empty
-- 
getTags :: AppHandler ()
getTags = findAllTags 
          >>= toJSONResponse

------------------------------------------------------------------------------

-- | Save a list of tags and return them getting ID has been insert.
--   Perform save if is new otherwise ignore.
--
saveTags :: [T.Text] -> AppHandler [Tag]
saveTags input = do
  xs <- findSomeTagsName input
  ys <- mapM insertTag $ filterExistsTags maybeNewTags xs
  return (xs ++ ys)
  where maybeNewTags = map textToTag input
        textToTag name = emptyTag { _tagName = name }

filterExistsTags :: [Tag]    -- ^ Tags input from web
                    -> [Tag] -- ^ Exists Tags per input
                    -> [Tag] -- ^ Those new ones
filterExistsTags = deleteFirstsBy eqName
  where eqName x y = _tagName x == _tagName y


----------------------------------------
