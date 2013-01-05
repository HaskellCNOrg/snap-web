{-# LANGUAGE OverloadedStrings    #-}

module Models.Feed
where

import qualified Data.Text              as T
import           Data.Time              (UTCTime)

import           Models.Topic
import           Models.Reply


data Feed = Feed
    { feedTitle    :: T.Text
    , feedLinkSelf :: T.Text
    , feedLinkHome :: T.Text
    , feedUpdated  :: UTCTime
    , feedEntries  :: [FeedEntry]
    }

data FeedEntry = FeedEntry
    { feedEntryTitle     :: T.Text
    , feedEntryLink      :: T.Text
    , feedEntryId        :: T.Text
    , feedEntryPublished :: UTCTime
    , feedEntryContent   :: T.Text
    }

topicToFeed :: [Topic] -> Feed
topicToFeed ts = 
    Feed { feedTitle = "HaskellCNOrg Topics"
         -- TODO: retrieve host address here
         , feedLinkSelf = "/feed/topic"
         , feedLinkHome = "/"
         , feedUpdated = _updateAt $ head ts
         , feedEntries = map topicToFeedEntry ts
         }
  where
    topicToFeedEntry t = 
        FeedEntry { feedEntryTitle = _title t
                  , feedEntryLink  = T.concat ["/topic/", getTopicId t]
                  , feedEntryId    = T.concat ["/topic/", getTopicId t]
                  , feedEntryPublished = _createAt t
                  , feedEntryContent = _content t
                  }

replyToFeed :: [Reply] -> [Topic] -> Feed
replyToFeed rs ts =
    Feed { feedTitle = "HaskellCNOrg Comments"
         , feedLinkSelf = "/feed/comment"
         , feedLinkHome = "/"
         , feedUpdated = _replyCreateAt $ head rs
         , feedEntries = zipWith replyToFeedEntry rs ts
         }
  where
    replyToFeedEntry r t = 
        FeedEntry { feedEntryTitle = T.concat ["Comments on ", _title t]
                  , feedEntryLink  = T.concat ["/topic/", getTopicId t]
                  , feedEntryId    = T.concat ["/topic/", getReplyId r]
                  , feedEntryPublished = _replyCreateAt r
                  , feedEntryContent = _replyContent r
                  }
