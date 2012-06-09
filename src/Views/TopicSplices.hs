{-# LANGUAGE OverloadedStrings, ExtendedDefaultRules #-}

module Views.TopicSplices
       ( topicSplices 
       , topicDetailSplices
       , replySplice ) where

import           Control.Arrow (second)
import           Control.Monad.Trans
import           Data.Maybe (isJust)
import           Text.Templating.Heist
import qualified Data.Text as T

import Application
import Models.Exception
import Models.Topic
import Models.Reply
import Models.User
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
      , ("oid", getTopicId tag) ]

-- | @Deprecated with @renderTopicWithReply@
-- 
renderTopic :: Topic -> Splice AppHandler
renderTopic tag = do
    rs <- lift $ findReplyPerTopic (textToObjectId $ getTopicId tag)
    runChildrenWith $
      map (second textSplice) [ ("topicTitle", _title tag)
                              , ("topicAuthor", _author tag)
                              , ("topicCreateAt", formatUTCTime $ _createAt tag)
                              , ("topicUpdateAt", formatUTCTime $ _updateAt tag)
                              , ("topicId", getTopicId tag) ]
      ++ [ ("topicContent", markdownToHtmlSplice $ _content tag)
         , ("replyPerTopic", allReplyPerTopicSplice rs) ]


allReplyPerTopicSplice :: [Reply] -> Splice AppHandler -- [(T.Text, Splice AppHandler)]
allReplyPerTopicSplice = mapSplices replySplice

replySplice :: Reply -> Splice AppHandler
replySplice r = do
                  usr <- lift$ findOneUser (_replyAuthor r)
                  runChildrenWithText 
                    [ ("replyAuthor",   _userDisplayName usr)
                    , ("replyId", getReplyId r)
                    , ("replyToTopicId", sToText $ _replyToTopicId r)
                    , ("replyToReplyId", objectIdToText $ _replyToReplyId r)
                    , ("replyCreateAt", formatUTCTime $ _replyCreateAt r)
                    , ("replyContent", _replyContent r) ]
          