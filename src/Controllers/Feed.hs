{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE OverloadedStrings    #-}

module Controllers.Feed
where

import qualified Data.ByteString as BS
import           Snap.Core       (writeBS, writeBuilder)
import           Snap.Snaplet    (Handler)

import           Application
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
    topics <- findAllTopic
    writeBuilder $ renderFeed topics


-- | Atom feed of comments.
--
commentFeed :: AppHandler ()
commentFeed = writeBS "comments feed"
