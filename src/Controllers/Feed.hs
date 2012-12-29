{-# LANGUAGE OverloadedStrings    #-}

module Controllers.Feed
where

import qualified Data.ByteString       as BS
import Snap.Core        (writeBS)
import Snap.Snaplet     (Handler)

import Application


routes :: [(BS.ByteString, Handler App App ())]
routes =  [ ("/feed/topic", topicFeed)
          , ("/feed/comment", commentFeed)
          ]


-- | Atom feed of topics.
-- 
topicFeed :: AppHandler ()
topicFeed = writeBS "topics feed"


-- | Atom feed of comments.
-- 
commentFeed :: AppHandler ()
commentFeed = writeBS "comments feed"
