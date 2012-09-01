{-# LANGUAGE OverloadedStrings, ExtendedDefaultRules #-}

module Models.Tag where

import           Control.Applicative ((<$>), (<*>))
import           Control.Monad.CatchIO (throw)
import           Data.Baeson.Types
import           Data.Bson
import           Data.Maybe (catMaybes)
import           Database.MongoDB
import           Snap.Snaplet.Auth
import qualified Data.Text as T

import           Application
import           Models.Utils
import           Models.Internal.Types

-- | Tag model
-- 
data Tag = Tag
    { _tagId      :: Maybe ObjectId
    , _tagName    :: T.Text
    , _tagContent :: Maybe T.Text
    } deriving (Show, Eq)

emptyTag :: Tag
emptyTag = Tag Nothing "" Nothing

-- | Schema name
-- 
tagCollection :: Collection
tagCollection = u "tags"


--------------------------------------------------------------------------------
-- Shortcuts
--------------------------------------------------------------------------------

-- | Get Tag Id in {Text}.
-- 
getTagId :: Tag -> T.Text
getTagId = objectIdToText . _tagId

toTagIds :: [Tag] -> Maybe [ObjectId]
toTagIds = Just . catMaybes . fmap _tagId


--------------------------------------------------------------------------------
-- Impl of Persistent Interface
--------------------------------------------------------------------------------

-- Impl of Persistent Interface
-- 
instance MongoDBPersistent Tag where
  mongoColl _  = tagCollection
  toMongoDoc   = tagToDocument
  fromMongoDoc = tagFromDocumentOrThrow
  mongoInsertId tag v = tag { _tagId = objectIdFromValue v }
  mongoGetId = _tagId

--------------------------------------------------------------------------------
-- CRUD
--------------------------------------------------------------------------------


-- | Insert a new tag.
--   meaning insert it if its new (has no "_id" field) or update it if its not new (has "_id" field)
-- | FIXME: couple of thoughts: 
--   2. batch create new tags?
--
insertTag :: Tag -> AppHandler Tag
insertTag = mongoInsert


findAllTags :: AppHandler [Tag]
findAllTags  = mongoFindAll emptyTag

findOneTag :: ObjectId -> AppHandler Tag
findOneTag oid = mongoFindOne $ emptyTag { _tagId = Just oid }

findSomeTags :: [ObjectId] -> AppHandler [Tag]
findSomeTags = mapM findOneTag -- FIXME: mongoFindSome emptyTag

--------------------------------------------------------------------------------
-- MongoDB Document Transform
--------------------------------------------------------------------------------


-- | Transform @Tag@ to mongoDB document.
--   Nothing of id mean new topic thus empty "_id" let mongoDB generate objectId.
-- 
tagToDocument :: Tag -> Document
tagToDocument tag = case _tagId tag of 
                          Nothing -> docs
                          Just x  -> ("_id" .= x) : docs
                    where docs = [ "name"     .= _tagName tag
                                 , "content"  .= _tagContent tag
                                 ]

-- | Transform mongo Document to be a Tag parser.
-- 
documentToTag :: Document -> Parser Tag
documentToTag d = Tag
                  <$> d .: "_id"
                  <*> d .: "name"
                  <*> d .: "content"

-- | parse the tag document
-- 
tagFromDocumentOrThrow :: Document -> IO Tag
tagFromDocumentOrThrow d = case parseEither documentToTag d of
    Left e  -> throw $ BackendError $ show e
    Right r -> return r
