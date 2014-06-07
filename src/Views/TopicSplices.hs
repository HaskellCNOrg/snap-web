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
import           Data.Time
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
                -> Splices (I.Splice AppHandler)
topicSplices topics page = foldSplices [ ("homeTopics", allTopicsSplice topics page)
                                       , ("ifNoTopics", ifNoTopicsSplice (length topics)) ]

allTopicsSplice :: Integral a
                   => [Topic]
                   -> Maybe a
                   -> I.Splice AppHandler
allTopicsSplice topics page = do
    let t = filter (isJust . _topicId) topics
    (i, xs, splice) <- lift $ paginationHandler currentPage' t
    I.runChildrenWith $ foldSplices [ ("allTopics", I.mapSplices renderTopicSimple xs)
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
topicDetailSplices :: Either UserException Topic -> Splices (I.Splice AppHandler)
topicDetailSplices = eitherToSplices


-----------------------------------------------------------------------------

-- | Single Topic to Splice
--
renderTopicSimple :: Topic -> I.Splice AppHandler
renderTopicSimple tag = do
    usr <- findTopicAuthor tag
    now <- liftIO getCurrentTime
    I.runChildrenWith $ foldSplices $
      map (second I.textSplice) (topicToSpliceContent tag usr now)

-- | Render a Topic with its replies.
--
renderTopic :: Topic -> I.Splice AppHandler
renderTopic topic = do
    now <- liftIO getCurrentTime
    rs <- lift $ findReplyPerTopic (textToObjectId $ getTopicId topic)
    user <- findTopicAuthor topic
    I.runChildrenWith $ foldSplices $
      map (second I.textSplice) (topicToSpliceContent topic user now)
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
topicToSpliceContent :: Topic -> User -> UTCTime -> [(T.Text, T.Text)]
topicToSpliceContent topic user now =
  [ ("topicTitle", _title topic)
  , ("topicAuthor", _userDisplayName user)
  , ("topicAuthorId", sToText $ _author topic)
  , ("topicCreateAt", relativeUTCTime (_createAt topic) now)
  , ("topicUpdateAt", relativeUTCTime (_updateAt topic) now)
  , ("topicId", getTopicId topic) ]
