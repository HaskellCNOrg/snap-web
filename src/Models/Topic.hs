{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE OverloadedStrings    #-}

module Models.Topic where

import           Control.Applicative   ((<$>), (<*>))
import           Control.Monad.CatchIO (throw)
import           Control.Monad.State
import           Data.Baeson.Types
import           Data.Bson
import qualified Data.Bson             as BSON
import qualified Data.Text             as T
import           Data.Time             (UTCTime)
import           Database.MongoDB
import qualified Database.MongoDB      as DB
import           Snap.Snaplet.Auth
import           Snap.Snaplet.MongoDB

import           Application
import           Models.Exception
import           Models.Internal.Types
import           Models.Utils


-- |
--
data Topic = Topic
    { _topicId   :: Maybe ObjectId
    , _title     :: T.Text
    , _content   :: T.Text
    , _author    :: ObjectId
    , _createAt  :: UTCTime
    , _updateAt  :: UTCTime
    , _topicTags :: Maybe [ObjectId]
    } deriving (Show, Eq)

topicCollection :: Collection
topicCollection = u "topics"

getTopicId :: Topic -> T.Text
getTopicId = objectIdToText . _topicId

-- | A very empty @Topic@
--
emptyTopic :: Topic
emptyTopic = Topic { _topicId = Nothing }


--------------------------------------------------------------------------------
-- Impl of Persistent Interface
--------------------------------------------------------------------------------

instance MongoDBPersistent Topic where
  mongoColl _  = topicCollection
  toMongoDoc   = topicToDocument
  fromMongoDoc = topicFromDocumentOrThrow
  mongoInsertId topic v = topic { _topicId = objectIdFromValue v }
  mongoGetId = _topicId

-- | Transform @Topic@ to mongoDB document.
--   Nothing of id mean new topic thus empty "_id" let mongoDB generate objectId.
--
topicToDocument :: Topic -> Document
topicToDocument topic = case _topicId topic of
                          Nothing -> docs
                          Just x  -> ("_id" .= x) : docs
                        where docs =
                                [  "title"    .= _title topic
                                , "content"   .= _content topic
                                , "author_id" .= _author topic
                                , "create_at" .= _createAt topic
                                , "update_at" .= _updateAt topic
                                , "tags"      .= _topicTags topic
                                ]

-- | Transform mongo Document to be a Topic parser.
--
documentToTopic :: Document -> Parser Topic
documentToTopic d = Topic
                    <$> d .: "_id"
                    <*> d .: "title"
                    <*> d .: "content"
                    <*> d .: "author_id"
                    <*> d .: "create_at"
                    <*> d .: "update_at"
                    <*> d .:? "tags"

-- | Parse the topic document
--
topicFromDocumentOrThrow :: Document -> IO Topic
topicFromDocumentOrThrow d = case parseEither documentToTopic d of
    Left e  -> throw $ BackendError $ show e
    Right r -> return r



--------------------------------------------------------------------------------
-- CRUD
--------------------------------------------------------------------------------


-- | create a new topic.
--
createNewTopic ::  Topic -> AppHandler Topic
createNewTopic = mongoInsert


-- | save a new topic.
--
saveTopic :: Topic -> AppHandler Topic
saveTopic = mongoSave


-- | Find One Topic by id.
--
findOneTopic :: ObjectId -> AppHandler Topic
findOneTopic oid = mongoFindById $ emptyTopic { _topicId = Just oid }


-- | Find All Topic.
--
findAllTopic :: AppHandler [Topic]
findAllTopic  = findTopicGeneric []


-- | Find topic per tag.
--
findTopicByTag :: ObjectId                   -- ^ Tag ID
                  -> AppHandler [Topic]
findTopicByTag tagId = findTopicGeneric  [ "tags" =: tagId ]


-- | Even generic find handler
--
findTopicGeneric :: Selector -> AppHandler [Topic]
findTopicGeneric se = do
  let topicSelection = select se topicCollection
  mongoFindAllBy emptyTopic (topicSelection {sort = sortByCreateAtDesc})


-- | Order by CreateAt column DESC.
--
sortByCreateAtDesc :: Order
sortByCreateAtDesc = [ "create_at" =: -1 ]

------------------------------------------------------------------------------

