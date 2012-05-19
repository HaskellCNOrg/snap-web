{-# LANGUAGE OverloadedStrings, ExtendedDefaultRules #-}

module Models.Topic where

import           Control.Applicative ((<$>), (<*>))
import           Control.Monad.State
import           Snap.Snaplet.Auth
import           Snap.Snaplet.MongoDB
--import           Control.Monad.Trans
import           Control.Monad.CatchIO (throw)
import qualified Data.Text as T
import           Data.Bson
import           Data.Baeson.Types
import Database.MongoDB

import           Application
import           Models.Exception
import Models.Types

-- | 
-- 
data Topic = Topic
    { _topicId :: ObjectId
    , _title   :: T.Text
    , _content :: T.Text
    , _author  :: T.Text
    } deriving (Show)

topicCollection :: Collection
topicCollection = u "topics"

------------------------------------------------------------------------------

-- | create a new topic. 
--  
createNewTopic ::  Topic -> AppHandler Topic
createNewTopic topic = do
    res <- eitherWithDB $ insert topicCollection $ makeTopicDocument topic
    either throwUE (\e->return topic) res 


------------------------------------------------------------------------------

-- | Find One Topic
-- 
findOneTopic :: ObjectId -> AppHandler (Maybe Topic)
findOneTopic oid = do
    res <- eitherWithDB $ findOne (select [ "_id" =: oid ] topicCollection)
--    either throwUE (\e->return topic) res 
    case res of
      Left x -> return Nothing
      Right Nothing -> return Nothing
      Right (Just y) -> liftIO $ fmap Just $ usrFromMongThrow y

usrFromMongThrow :: Document -> IO Topic
usrFromMongThrow d =  case parseEither documentToTopic d of
  Left e -> throw $ BackendError $ show e
  Right r -> return r
  
------------------------------------------------------------------------------

-- | Find All Topic
-- 
findAllTopic :: AppHandler [Topic]
findAllTopic  = do
    xs <- eitherWithDB $ rest =<< find (select [] topicCollection)
    liftIO $ (mapM usrFromMongThrow) $ either (const []) id xs  

  
------------------------------------------------------------------------------

makeTopicDocument :: Topic -> Document
makeTopicDocument topic = [ "_id" =: _topicId topic ]
                          ++ 
                          [ "title"   .= _title topic
                          , "content" .= _content topic
                          , "author"  .= _author topic
                          ]

documentToTopic :: Document -> Parser Topic
documentToTopic d = Topic
                <$> d .: "_id"
                <*> d .: "title"
                <*> d .: "content"
                <*> d .: "author"

------------------------------------------------------------------------------
