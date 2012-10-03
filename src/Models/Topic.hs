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
import           Models.Utils

--import           Control.Monad.Trans

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


------------------------------------------------------------------------------

-- | create a new topic.
--
createNewTopic ::  Topic -> AppHandler Topic
createNewTopic topic = do
    res <- eitherWithDB $ DB.insert topicCollection $ topicToDocument topic
    either failureToUE (return . insertId') res
  where insertId' x = topic { _topicId = BSON.cast' x}

-- | save a new topic.
--   meaning insert it if its new (has no "_id" field) or update it if its not new (has "_id" field)
--
--   FIXME: 1. better to make sure _id exists because Nothing objectId will cause error other when viewing.
--          2. why have createNewTopic and saveTopi?
--
saveTopic :: Topic -> AppHandler Topic
saveTopic topic = do
    res <- eitherWithDB $ DB.save topicCollection $ topicToDocument topic
    either failureToUE (const $ return topic) res

------------------------------------------------------------------------------

-- | Find One Topic
--
findOneTopic :: ObjectId -> AppHandler Topic
findOneTopic oid = do
    res <- eitherWithDB $ fetch (select [ "_id" =: oid ] topicCollection)
    either failureToUE (liftIO . topicFromDocumentOrThrow) res

------------------------------------------------------------------------------

-- | Find All Topic.
--   FIXME: pagination.
--
findAllTopic :: AppHandler [Topic]
findAllTopic  = do
    let topicSelection = select [] topicCollection
    xs <- eitherWithDB $ rest =<< find (topicSelection {sort = sortByUpdateAtDesc} )
    liftIO $ mapM topicFromDocumentOrThrow $ either (const []) id xs

sortByUpdateAtDesc :: Order
sortByUpdateAtDesc = [ "createAt" =: -1 ]

------------------------------------------------------------------------------

-- | Transform @Topic@ to mongoDB document.
--   Nothing of id mean new topic thus empty "_id" let mongoDB generate objectId.
--
topicToDocument :: Topic -> Document
topicToDocument topic = case _topicId topic of
                          Nothing -> docs
                          Just x  -> ("_id" .= x) : docs
                        where docs =
                                [  "title"   .= _title topic
                                , "content"  .= _content topic
                                , "author"   .= _author topic
                                , "createAt" .= _createAt topic
                                , "updateAt" .= _updateAt topic
                                , "tags"     .= _topicTags topic
                                ]

-- | Transform mongo Document to be a Topic parser.
--
documentToTopic :: Document -> Parser Topic
documentToTopic d = Topic
                    <$> d .: "_id"
                    <*> d .: "title"
                    <*> d .: "content"
                    <*> d .: "author"
                    <*> d .: "createAt"
                    <*> d .: "updateAt"
                    <*> d .: "tags"

-- | parse the topic document
--
topicFromDocumentOrThrow :: Document -> IO Topic
topicFromDocumentOrThrow d = case parseEither documentToTopic d of
    Left e  -> throw $ BackendError $ show e
    Right r -> return r


------------------------------------------------------------------------------
