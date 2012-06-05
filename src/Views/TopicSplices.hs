{-# LANGUAGE OverloadedStrings, ExtendedDefaultRules #-}

module Views.TopicSplices
       ( topicSplices 
       , topicDetailSplices ) where

import           Control.Arrow (second)
import           Control.Monad.Trans
import           Data.Maybe (isJust)
import           Text.Templating.Heist
import qualified Data.Text as T
import           Data.Bson (ObjectId)

import Application
import Models.Exception
import Models.Topic
import Models.Reply
import Views.MarkdownSplices
import Views.Types
import Views.Utils
import Models.Utils


------------------------------------------------------------------------------

instance SpliceRenderable Topic where
   toSplice = renderTopic

------------------------------------------------------------------------------
                    
-- | display all topics.
-- 

-- FIXME: what if no topics at all??
-- 
topicSplices :: [(T.Text, Splice AppHandler)]
topicSplices = [("allTopics", allTopicsSplice)]

allTopicsSplice :: Splice AppHandler
allTopicsSplice = do
    t <- lift findAllTopic  
    mapSplices renderTopicSimple $ filter (isJust . _topicId) t

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
renderTopicSimple tag = runChildrenWithText 
      [ ("topicTitle", _title tag)
      , ("topicAuthor", _author tag)
      , ("oid", topicIdToText tag) ]

-- | @Deprecated with @renderTopicWithReply@
-- 
renderTopic :: Topic -> Splice AppHandler
renderTopic tag = do
    rs <- lift $ findReplyPerTopic (textToObjectId $ topicIdToText tag)
    runChildrenWith $
      map (second textSplice) [ ("topicTitle", _title tag)
                              , ("topicAuthor", _author tag)
                              , ("topicCreateAt", formatUTCTime $ _createAt tag)
                              , ("topicUpdateAt", formatUTCTime $ _updateAt tag)
                              , ("oid", topicIdToText tag) ]
      ++ [ ("topicContent", markdownToHtmlSplice $ _content tag)
         , ("replyPerTopic", allReplyPerTopicSplice rs) ]


allReplyPerTopicSplice :: [Reply] -> Splice AppHandler -- [(T.Text, Splice AppHandler)]
allReplyPerTopicSplice rs = mapSplices ss' rs
    where ss' r = runChildrenWithText 
                    [ ("replyAuthor", _replyAuthor r)
                    , ("replyCreateAt", formatUTCTime $ _replyCreateAt r)
                    , ("replyContent", _replyContent r) ]
