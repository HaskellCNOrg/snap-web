{-# LANGUAGE OverloadedStrings, ExtendedDefaultRules #-}

module Views.TopicSplices
       ( topicSplices 
       , topicDetailSplices ) where

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
import Views.ReplySplices
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
renderTopicSimple tag = do
    usrName <- findTopicAuthorName tag
    runChildrenWithText 
        [ ("topicTitle", _title tag)
        , ("topicAuthor", usrName)
        , ("oid", getTopicId tag) ]

-- | @Deprecated with @renderTopicWithReply@
-- 
renderTopic :: Topic -> Splice AppHandler
renderTopic tag = do
    rs <- lift $ findReplyPerTopic (textToObjectId $ getTopicId tag)
    usrName <- findTopicAuthorName tag
    runChildrenWith $
      map (second textSplice) [ ("topicTitle", _title tag)
                              , ("topicAuthor", usrName)
                              , ("topicCreateAt", formatUTCTime $ _createAt tag)
                              , ("topicUpdateAt", formatUTCTime $ _updateAt tag)
                              , ("topicId", getTopicId tag) ]
      ++ [ ("topicContent", markdownToHtmlSplice $ _content tag)
         , ("replyPerTopic", allReplyPerTopicSplice rs) ]

-- | @Splice@ is type synonium as @Splice m = HeistT m Template@
-- 
findTopicAuthorName :: Topic -> HeistT AppHandler T.Text
findTopicAuthorName topic = lift (findUser' topic) >>= return . _userDisplayName
                            where findUser' = findOneUser . _author
