{-# LANGUAGE OverloadedStrings, ExtendedDefaultRules #-}

module Models.Reply where

import           Control.Applicative ((<$>), (<*>))
import           Control.Monad.CatchIO (throw)
import           Control.Monad.State
import           Data.Baeson.Types
import           Data.Bson
import           Database.MongoDB
import qualified Database.MongoDB as DB
import           Snap.Snaplet.Auth
import           Snap.Snaplet.MongoDB
import qualified Data.Text as T
import           Data.Time (UTCTime)

import           Application
import           Models.Exception
--import           Models.Types


--import           Control.Monad.Trans

-- | 
-- 
data Reply = Reply
    { _replyId        :: Maybe ObjectId  -- ^ Reply obj id
    , _replyToTopicId :: ObjectId        -- ^ The Topic that reply to
    , _replyToReplyId :: Maybe ObjectId  -- ^ maybe reply to a reply
    , _replyContent   :: T.Text
    , _replyAuthor    :: ObjectId
    , _replyCreateAt  :: UTCTime
    -- MAYBE: column for soft deletion.
    } deriving (Show)

replyCollection :: Collection
replyCollection = u "replies"

-----------------------------------------------------------------------

-- | Save a reply which is to reply.
--  
createReplyToTopic :: Reply -> AppHandler ()
createReplyToTopic reply = do
    res <- eitherWithDB $ DB.insert replyCollection $ replyToDocument reply
    either failureToUE (const $ return ()) res


-----------------------------------------------------------------------

-- | Save a reply which is to reply.
--  
findReplyPerTopic :: ObjectId -> AppHandler [Reply]
findReplyPerTopic tid = do
    let queryReply = select [ "topic_id" =: tid ] replyCollection
    res <- eitherWithDB $ rest =<< find (queryReply { sort = sortByCreateAtDesc })
    liftIO $ mapM replyFromDocumentOrThrow $ either (const []) id res

sortByCreateAtDesc :: Order
sortByCreateAtDesc = [ "create_at" =: -1 ]

------------------------------------------------------------------------------

-- | Transform @Reply@ to mongoDB document.
--   Nothing of id mean new reply thus empty "_id" let mongoDB generate objectId.
-- 
replyToDocument :: Reply -> Document
replyToDocument reply = case _replyId reply of 
                          Nothing -> docs
                          Just x  -> ("_id" .= _replyId reply) : docs
                        where docs = 
                                [ "topic_id"  .= _replyToTopicId reply
                                , "reply_id"   .= _replyToReplyId reply
                                , "content"    .= _replyContent reply
                                , "author_id"  .= _replyAuthor reply
                                , "create_at"  .= _replyCreateAt reply
                                ]

-- | Transform mongo Document to be a reply parser.
-- 
documentToreply :: Document -> Parser Reply
documentToreply d = Reply
                    <$> d .: "_id"
                    <*> d .: "topic_id"
                    <*> d .: "reply_id"
                    <*> d .: "content"
                    <*> d .: "author_id" 
                    <*> d .: "create_at"

---- | parse the reply document
---- 
replyFromDocumentOrThrow :: Document -> IO Reply
replyFromDocumentOrThrow d = case parseEither documentToreply d of
    Left e  -> throw $ BackendError $ show e
    Right r -> return r
