{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE OverloadedStrings    #-}

module Controllers.Feed
where

import           Control.Monad   (mapM)
import qualified Data.ByteString as BS
import           Snap.Core       (writeBS, writeBuilder)
import           Snap.Snaplet    (Handler)

import           Application
import           Models.Topic
import           Models.Reply
import           Views.Feed


routes :: [(BS.ByteString, Handler App App ())]
routes =  [ ("/feed/topic", topicFeed)
          , ("/feed/comment", commentFeed)
          ]


-- | Atom feed of topics.
--
topicFeed :: AppHandler ()
topicFeed = do
    topics <- findAllTopic
    writeBuilder $ renderTopicFeed topics


-- | Atom feed of comments.
--
commentFeed :: AppHandler ()
commentFeed = do
    replys <- findAllReply
    --topics <- (findOneTopic . _replyToTopicId ) $ head replys 
    topics <- mapM (findOneTopic . _replyToTopicId) replys
    --writeBS "comments feed"
    writeBuilder $ renderReplyFeed replys topics
