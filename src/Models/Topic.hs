{-# LANGUAGE OverloadedStrings, ExtendedDefaultRules #-}

module Models.Topic where

import           Control.Applicative ((<$>), (<*>))
import           Control.Monad.CatchIO (throw)
import           Control.Monad.State
import           Data.Baeson.Types
import           Data.Bson
import           Database.MongoDB
import           Snap.Snaplet.Auth
import           Snap.Snaplet.MongoDB
import qualified Data.Text as T
import           Data.Time (UTCTime)

import           Application
import           Models.Exception
import           Models.Types

--import           Control.Monad.Trans

-- | 
-- 
data Topic = Topic
    { _topicId :: ObjectId
    , _title   :: T.Text
    , _content :: T.Text
    , _author  :: T.Text
    , _createAt :: UTCTime
    , _updateAt :: UTCTime
    } deriving (Show)

{-
	last_reply: { type: ObjectId },
	last_reply_at: { type: Date, default: Date.now },
-}

topicCollection :: Collection
topicCollection = u "topics"

------------------------------------------------------------------------------

-- | create a new topic. 
--  
createNewTopic ::  Topic -> AppHandler Topic
createNewTopic topic = do
    res <- eitherWithDB $ insert topicCollection $ topicToDocument topic
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
    xs <- eitherWithDB $ rest =<< find (select [] topicCollection)
    liftIO $ mapM topicFromDocumentOrThrow $ either (const []) id xs  

  
------------------------------------------------------------------------------

-- | Transform @Topic@ to mongoDB document.
-- 
topicToDocument :: Topic -> Document
topicToDocument topic = [ "_id"     .= _topicId topic 
                        , "title"   .= _title topic
                        , "content" .= _content topic
                        , "author"  .= _author topic
                        , "createAt" .= _createAt topic
                        , "updateAt" .= _updateAt topic
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

topicFromDocumentOrThrow :: Document -> IO Topic
topicFromDocumentOrThrow d = case parseEither documentToTopic d of
    Left e  -> throw $ BackendError $ show e
    Right r -> return r
  

------------------------------------------------------------------------------
