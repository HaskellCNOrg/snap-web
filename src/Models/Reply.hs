{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE OverloadedStrings    #-}

module Models.Reply where


import           Application
import           Control.Applicative       ((<$>), (<*>))
import           Control.Monad.CatchIO     (throw)
import           Data.Baeson.Types
import           Data.Bson
import           Data.Maybe                (isJust, isNothing)
import qualified Data.Text                 as T
import           Data.Time                 (UTCTime)
import           Database.MongoDB
import qualified Database.MongoDB          as DB
import           Models.Internal.Exception
import           Models.Internal.Types
import           Models.Utils
import           Snap.Snaplet.MongoDB


data Reply = Reply
    { _replyId        :: Maybe ObjectId  -- ^ Reply obj id
    , _replyToTopicId :: ObjectId        -- ^ The Topic that reply to
    , _replyToReplyId :: Maybe ObjectId  -- ^ maybe reply to a reply
    , _replyContent   :: T.Text
    , _replyAuthor    :: ObjectId
    , _replyCreateAt  :: UTCTime
    } deriving (Show, Eq)

replyCollection :: Collection
replyCollection = "replies"

emptyReply :: Reply
emptyReply = Reply { _replyId = Nothing }


--------------------------------------------------------------------------------
-- Implement of Mongo Persistent
--------------------------------------------------------------------------------

instance MongoDBPersistent Reply where
  mongoColl _  = replyCollection
  toMongoDoc   = replyToDocument
  fromMongoDoc = replyFromDocumentOrThrow
  mongoInsertId reply v = reply { _replyId = objectIdFromValue v }
  mongoGetId = _replyId


--------------------------------------------------------------------------------
-- CRUD
--------------------------------------------------------------------------------

-- | Save a reply which is to reply.
--
createReplyToTopic :: Reply -> AppHandler Reply
createReplyToTopic = mongoInsert
--    res <- eitherWithDB $ DB.insert replyCollection $ replyToDocument reply
--    either failureToUE (return . updateOid) res
--    where updateOid v = reply { _replyId = objectIdFromValue v }


-- | Find all Replies under a topic.
--
findReplyPerTopic :: ObjectId -> AppHandler [Reply]
findReplyPerTopic tid = do
    let query' = select [ "topic_id" =: tid ] replyCollection
        sort' = [ "create_at" =: 1, "reply_id" =: 1 ]
    mongoFindAllBy emptyReply (query' { sort = sort' })

--    res <- eitherWithDB $ rest =<< find (
--    liftIO $ mapM replyFromDocumentOrThrow $ either (const []) id res


findAllReply :: AppHandler [Reply]
findAllReply =
    let query' = select [] replyCollection
        sort' = [ "create_at" =: -1, "reply_id" =: -1 ]
    in
     mongoFindAllBy emptyReply (query' { sort = sort' })

-- | Delete a reply
--
deleteReply :: ObjectId -> AppHandler ()
deleteReply rid =
    eitherWithDB (DB.delete (select query replyCollection))
    >>= either failureToUE (const $ return ())
    where query = [ "$or" =: [ [ "_id" =: rid ], ["reply_id" =: rid] ] ]


--------------------------------------------------------------------------------
-- mongo document transform
--------------------------------------------------------------------------------

-- | Transform @Reply@ to mongoDB document.
--   Nothing of id mean new reply thus empty "_id" let mongoDB generate objectId.
--
replyToDocument :: Reply -> Document
replyToDocument reply = case _replyId reply of
                          Nothing -> docs
                          Just _  -> ("_id" .= _replyId reply) : docs
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
                    <*> d .:? "reply_id"
                    <*> d .: "content"
                    <*> d .: "author_id"
                    <*> d .: "create_at"

---- | parse the reply document
----
replyFromDocumentOrThrow :: Document -> IO Reply
replyFromDocumentOrThrow d = case parseEither documentToreply d of
    Left e  -> throw $ UserException e
    Right r -> return r

--------------------------------------------------------------------------------
-- Shortcuts
--------------------------------------------------------------------------------

getReplyId :: Reply -> T.Text
getReplyId = objectIdToText . _replyId

-- | All Replies but not ReplyToReply
--
firstLevelReply :: [Reply] -> [Reply]
firstLevelReply = filter (isNothing . _replyToReplyId)

-- | not . firstLevelReply
--
nonFirstLevelReply :: [Reply] -> [Reply]
nonFirstLevelReply = filter (isJust . _replyToReplyId)
