{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE OverloadedStrings    #-}

module Controllers.Feed
where

import           Control.Monad   (mapM)
import qualified Data.ByteString as BS
import           Snap.Core       (writeBuilder)
import           Snap.Snaplet    (Handler)
import           Snap.Snaplet.Environments

import           Application
import           Models.Feed
import           Models.Reply
import           Models.Topic
import           Views.Feed


routes :: [(BS.ByteString, Handler App App ())]
routes =  [ ("/feed/topic", topicFeed)
          , ("/feed/comment", commentFeed)
          ]


-- | Atom feed of topics.
--
topicFeed :: AppHandler ()
topicFeed = do
    count <- lookupConfigDefault "feed.topicMax" 20
    topics <- findAllTopic
    feed <- topicToFeed $ take count topics
    writeBuilder $ renderFeed feed


-- | Atom feed of comments.
--
commentFeed :: AppHandler ()
commentFeed = do
    count <- lookupConfigDefault "feed.commentMax" 20
    replys <- findAllReply
    feed <- replyToFeed $ take count replys
    writeBuilder $ renderFeed feed
