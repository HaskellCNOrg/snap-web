{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE OverloadedStrings    #-}

module Views.TopicSplices
       ( topicSplices
       , topicDetailSplices ) where

import           Application
import           Control.Arrow           (second)
import           Control.Monad.Trans
import           Data.Maybe              (isJust)
import qualified Data.Text               as T
import           Heist
import qualified Heist.Interpreted       as I
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
                -> [(T.Text, I.Splice AppHandler)]
topicSplices topics page = [ ("homeTopics", allTopicsSplice topics page)
                           , ("ifNoTopics", ifNoTopicsSplice (length topics)) ]

allTopicsSplice :: Integral a
                   => [Topic]
                   -> Maybe a
                   -> I.Splice AppHandler
allTopicsSplice topics page = do
    let t = filter (isJust . _topicId) topics
    (i, xs, splice) <- lift $ paginationHandler currentPage' t
    I.runChildrenWith
      [ ("allTopics", I.mapSplices renderTopicSimple xs)
      , ("pagination", splice)
      , ("startIndex", I.textSplice $ sToText i) ]
    where currentPage' :: Integral a => a
          currentPage' = maybe 1 fromIntegral page

ifNoTopicsSplice :: Int -> I.Splice AppHandler
ifNoTopicsSplice n = if n <= 0 then I.runChildren else return []

------------------------------------------------------------------------------

-- | Splices used at Topic Detail page.
--   Display either a topic or error msg.
--
topicDetailSplices :: Either UserException Topic -> [(T.Text, I.Splice AppHandler)]
topicDetailSplices = eitherToSplices


------------------------------------------------------------------------------

-- | Single Topic to Splice
--
renderTopicSimple :: Topic -> I.Splice AppHandler
renderTopicSimple tag = do
    usr <- findTopicAuthor tag
    I.runChildrenWithText (topicToSpliceContent tag usr)

-- | Render a Topic with its replies.
--
renderTopic :: Topic -> I.Splice AppHandler
renderTopic topic = do
    rs <- lift $ findReplyPerTopic (textToObjectId $ getTopicId topic)
    user <- findTopicAuthor topic
    I.runChildrenWith $
      map (second I.textSplice) (topicToSpliceContent topic user)
      ++ [ ("topicContent", markdownToHtmlSplice $ _content topic)
         , ("replyPerTopic", allReplyPerTopicSplice rs)
         , ("topicEditable", hasEditPermissionSplice user)
         , ("topicTagList", topicTagSplice $ _topicTags topic)]

------------------------------------------------------------------------------

-- | @Splice@ is type synonium as @Splice m = HeistT m Template@
--
findTopicAuthor :: Topic -> HeistT AppHandler AppHandler User
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
