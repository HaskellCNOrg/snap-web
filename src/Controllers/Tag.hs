{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE OverloadedStrings    #-}

module Controllers.Tag
       ( routes
       , saveTags
       , filterExistsTags
       ) where

import           Application
import qualified Data.ByteString       as BS
import           Data.List             (deleteFirstsBy, nub)
import qualified Data.Text             as T
import           Models.Tag
import           Snap
import           Snap.Snaplet.Heist
import           Text.Templating.Heist
import           Views.TagSplices
import           Views.Utils

------------------------------------------------------------------------------

-- | Routes
--  /tags -> get all tags
--
routes :: [(BS.ByteString, Handler App App ())]
routes =  [ ("/tags",  Snap.method GET getTagsH)
          ]

tplTagList :: BS.ByteString
tplTagList = "tag-list"

------------------------------------------------------------------------------

-- | Fetch all tags
--
-- MAYBE: 1. if content-tye is not json, return empty
--
getTagsH :: AppHandler ()
getTagsH = do
  req <- getRequest
  tags <- findAllTags
  let acceptJSON = hasAcceptHeaderJSON $ headers req
  if acceptJSON then toJSONResponse tags else
    heistLocal (bindSplice "tags" $ tagsSplice tags) $ render tplTagList

------------------------------------------------------------------------------

-- | Save a list of tags and return them getting ID has been insert.
--   Perform save if is new otherwise ignore.
--
--
saveTags :: [T.Text] -> AppHandler [Tag]
saveTags input = do
  let input' = nub input
  xs <- findSomeTagsName input'
  ys <- mapM insertTag $ filterExistsTags (maybeNewTags input') xs
  return (xs ++ ys)
  where maybeNewTags = map textToTag
        textToTag name = emptyTag { _tagName = name }

filterExistsTags :: [Tag]    -- ^ Tags input from web
                    -> [Tag] -- ^ Exists Tags per input
                    -> [Tag] -- ^ Those new ones
filterExistsTags = deleteFirstsBy eqName
  where eqName x y = _tagName x == _tagName y


----------------------------------------
