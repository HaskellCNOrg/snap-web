{-# LANGUAGE OverloadedStrings, ExtendedDefaultRules #-}

module Models.Topic where

import           Control.Applicative ((<$>), (<*>))
import           Control.Monad
import           Control.Monad.State
import           Snap.Core
import           Snap.Snaplet.Auth
import           Snap.Snaplet
import           Snap.Snaplet.Heist
import           Snap.Snaplet.I18N
-- import           Text.Digestive
-- import           Text.Digestive.Heist
import           Snap.Snaplet.MongoDB
import           Text.Digestive.Snap
import           Control.Monad.Trans
import           Text.Templating.Heist
import           Control.Monad.CatchIO (try, throw,  Exception(..))
import qualified Data.Text as T
import           Data.Bson
import           Data.Baeson.Types
import Data.Baeson.Types
import Database.MongoDB

import           Application
import           Models.Exception
import           Models.Utils
import           Models.Types

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
