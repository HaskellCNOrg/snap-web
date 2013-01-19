{-# LANGUAGE OverloadedStrings #-}

module Models.Feed
where

import           Data.Maybe
import qualified Data.Text    as T
import           Data.Time    (UTCTime)

import           Application
import Snap.Snaplet.I18N
import           Models.Reply
import           Models.Topic
import           Models.User
import           Models.Utils (sToText)


data Feed = Feed
    { feedTitle    :: T.Text
    , feedLinkSelf :: T.Text
    , feedLinkHome :: T.Text
    , feedUpdated  :: UTCTime
    , feedEntries  :: [FeedEntry]
    }

data FeedEntry = FeedEntry
    { feedEntryTitle      :: T.Text
    , feedEntryLink       :: T.Text
    , feedEntryId         :: T.Text
    , feedEntryPublished  :: UTCTime
    , feedEntryContent    :: T.Text
    , feedEntryAuthorName :: T.Text
    , feedEntryAuthorUri  :: T.Text
    }

topicToFeed :: [Topic] -> AppHandler Feed
topicToFeed ts = do
    entries <- mapM topicToFeedEntry ts
    feedTopicTitle <- lookupI18NValue "feed.topic.title"
    return
        Feed { feedTitle = feedTopicTitle
             -- TODO: retrieve host address here
             , feedLinkSelf = "/feed/topic"
             , feedLinkHome = "/"
             , feedUpdated = _updateAt $ head ts
             , feedEntries = entries
             }

topicToFeedEntry :: Topic -> AppHandler FeedEntry
topicToFeedEntry t = do
    u <- findOneUser $ _author t
    let authorId = sToText $ _author t
    return
        FeedEntry { feedEntryTitle = _title t
                  , feedEntryLink  = T.concat ["/topic/", getTopicId t]
                  , feedEntryId    = T.concat ["/topic/", getTopicId t]
                  , feedEntryPublished = _createAt t
                  , feedEntryContent = _content t
                  , feedEntryAuthorName = _userDisplayName u
                  , feedEntryAuthorUri = T.concat ["/user/", authorId]
                  }

replyToFeed :: [Reply] -> AppHandler Feed
replyToFeed rs = do
    entries <- mapM replyToFeedEntry rs
    feedCommentTitle <- lookupI18NValue "feed.comment.title"
    return
        Feed { feedTitle = feedCommentTitle
             , feedLinkSelf = "/feed/comment"
             , feedLinkHome = "/"
             , feedUpdated = _replyCreateAt $ head rs
             , feedEntries = entries
             }

replyToFeedEntry :: Reply -> AppHandler FeedEntry
replyToFeedEntry r = do
    t <- findOneTopic $ _replyToTopicId r
    u <- findOneUser $ _replyAuthor r
    let authorId = sToText $ _replyAuthor r
    return
        FeedEntry { feedEntryTitle = T.concat ["Comment on ", _title t]
                  , feedEntryLink  = T.concat ["/topic/", getTopicId t]
                  , feedEntryId    = T.concat ["/topic/", getReplyId r]
                  , feedEntryPublished = _replyCreateAt r
                  , feedEntryContent = _replyContent r
                  , feedEntryAuthorName = _userDisplayName u
                  , feedEntryAuthorUri = T.concat ["/user/", authorId]
                  }
