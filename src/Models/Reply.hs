{-# LANGUAGE OverloadedStrings, ExtendedDefaultRules #-}

module Models.Reply where

import           Control.Applicative ((<$>), (<*>))
import           Control.Monad.CatchIO (throw)
import           Control.Monad.State
import           Data.Baeson.Types
import           Data.Bson
import           Database.MongoDB
import qualified Data.Bson as BSON
import qualified Database.MongoDB as DB
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
data Reply = Reply
    { _replyId        :: Maybe ObjectId  -- ^ Reply obj id
    , _replyToTopicId :: ObjectId        -- ^ The Topic that reply to
    , _replyToReplyId :: Maybe ObjectId  -- ^ maybe reply to a reply
    , _replyContent   :: T.Text
    , _replyAuthor    :: T.Text
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


------------------------------------------------------------------------------

-- | Transform @Reply@ to mongoDB document.
--   Nothing of id mean new reply thus empty "_id" let mongoDB generate objectId.
-- 
replyToDocument :: Reply -> Document
replyToDocument reply = case _replyId reply of 
                          Nothing -> docs
                          Just x  -> ("_id" .= _replyId reply) : docs
                        where docs = 
                                [  "topic_id"  .= _replyToTopicId reply
                                , "reply_id"   .= _replyToReplyId reply
                                , "content"    .= _replyContent reply
                                , "author_id"  .= _replyAuthor reply
                                , "create_at"  .= _replyCreateAt reply
                                ]

-- | Transform mongo Document to be a reply parser.
-- 
--documentToreply :: Document -> Parser reply
--documentToreply d = reply
--                    <$> d .: "_id"
--                    <*> d .: "title"
--                    <*> d .: "content"
--                    <*> d .: "author"
--                    <*> d .: "createAt" 
--                    <*> d .: "updateAt"

---- | parse the reply document
---- 
--replyFromDocumentOrThrow :: Document -> IO reply
--replyFromDocumentOrThrow d = case parseEither documentToreply d of
--    Left e  -> throw $ BackendError $ show e
--    Right r -> return r
