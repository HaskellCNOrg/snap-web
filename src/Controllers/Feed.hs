{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE OverloadedStrings    #-}

module Controllers.Feed
where

import           Control.Monad   (mapM)
import qualified Data.ByteString as BS
import           Snap.Core       (writeBuilder)
import           Snap.Snaplet    (Handler)

import           Application
import           Models.Topic
import           Models.Reply
import           Models.Feed
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
    writeBuilder $ renderFeed $ topicToFeed topics


-- | Atom feed of comments.
--
commentFeed :: AppHandler ()
commentFeed = do
    replys <- findAllReply
    topics <- mapM (findOneTopic . _replyToTopicId) replys
    writeBuilder $ renderFeed $ replyToFeed replys topics
