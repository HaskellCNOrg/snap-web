{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE OverloadedStrings    #-}

module Views.TopicSplices
       ( topicSplices
       , topicDetailSplices ) where

import           Control.Arrow           (second)
import           Control.Monad
import           Control.Monad.Trans
import           Data.Maybe              (isJust)
import qualified Data.Text               as T
import           Text.Templating.Heist

import           Application
import           Models.Exception
import           Models.Reply
import           Models.Topic
import           Models.User
import           Models.Utils
import           Views.MarkdownSplices
import           Views.PaginationSplices
import           Views.ReplySplices
import           Views.TagSplices
import           Views.Types
import           Views.UserSplices
import           Views.Utils


------------------------------------------------------------------------------

instance SpliceRenderable Topic where
   toSplice = renderTopic

------------------------------------------------------------------------------

-- | display all topics.
--

-- FIXME: what if no topics at all??
--
topicSplices :: Integral a
                => [Topic]
                -> Maybe a
                -> [(T.Text, Splice AppHandler)]
topicSplices topics page = [ ("homeTopics", allTopicsSplice topics page)
                           , ("ifNoTopics", ifNoTopicsSplice (length topics)) ]

allTopicsSplice :: Integral a
                   => [Topic]
                   -> Maybe a
                   -> Splice AppHandler
allTopicsSplice topics page = do
    let t = filter (isJust . _topicId) topics
    (i, xs, splice) <- lift $ paginationHandler currentPage' t
    runChildrenWith
      [ ("allTopics", mapSplices renderTopicSimple xs)
      , ("pagination", splice)
      , ("startIndex", textSplice $ sToText i) ]
    where total' = fromIntegral . length
          currentPage' :: Integral a => a
          currentPage' = maybe 1 fromIntegral page

ifNoTopicsSplice :: Int -> Splice AppHandler
ifNoTopicsSplice n = if n <= 0 then runChildren else return []

------------------------------------------------------------------------------

-- | Splices used at Topic Detail page.
--   Display either a topic or error msg.
--
topicDetailSplices :: Either UserException Topic -> [(T.Text, Splice AppHandler)]
topicDetailSplices = eitherToSplices


------------------------------------------------------------------------------

-- | Single Topic to Splice
--
renderTopicSimple :: Topic -> Splice AppHandler
renderTopicSimple tag = do
    usr <- findTopicAuthor tag
    runChildrenWithText (topicToSpliceContent tag usr)

-- | Render a Topic with its replies.
--
renderTopic :: Topic -> Splice AppHandler
renderTopic topic = do
    rs <- lift $ findReplyPerTopic (textToObjectId $ getTopicId topic)
    user <- findTopicAuthor topic
    runChildrenWith $
      map (second textSplice) (topicToSpliceContent topic user)
      ++ [ ("topicContent", markdownToHtmlSplice $ _content topic)
         , ("replyPerTopic", allReplyPerTopicSplice rs)
         , ("topicEditable", hasEditPermissionSplice user)
         , ("topicTagList", topicTagSplice $ _topicTags topic)]

------------------------------------------------------------------------------

-- | @Splice@ is type synonium as @Splice m = HeistT m Template@
--
findTopicAuthor :: Topic -> HeistT AppHandler User
findTopicAuthor topic = lift (findUser' topic)
                        where findUser' = findOneUser . _author

-- findTopicAuthorName :: Topic -> HeistT AppHandler T.Text
-- findTopicAuthorName topic = liftM _userDisplayName (findTopicAuthor topic)


-- | Topic to Splice "VO"
--
topicToSpliceContent :: Topic -> User -> [(T.Text, T.Text)]
topicToSpliceContent topic user = [ ("topicTitle", _title topic)
                              , ("topicAuthor", _userDisplayName user)
                              , ("topicAuthorId", sToText $ _author topic)
                              , ("topicCreateAt", formatUTCTime $ _createAt topic)
                              , ("topicUpdateAt", formatUTCTime $ _updateAt topic)
                              , ("topicId", getTopicId topic) ]
