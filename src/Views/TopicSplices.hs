{-# LANGUAGE OverloadedStrings, ExtendedDefaultRules #-}

module Views.TopicSplices
       ( topicSplices 
       , topicDetailSplices ) where

import           Control.Arrow (second)
import           Control.Monad.Trans
import           Control.Monad
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
import Views.UserSplices
import Views.PaginationSplices
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
topicSplices = [ ("allTopics", allTopicsSplice)
               , ("pagination", paginationSplice 2) ]

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
    user <- findTopicAuthor tag
    runChildrenWith $
      map (second textSplice) [ ("topicTitle", _title tag)
                              , ("topicAuthor", _userDisplayName user)
                              , ("topicCreateAt", formatUTCTime $ _createAt tag)
                              , ("topicUpdateAt", formatUTCTime $ _updateAt tag)
                              , ("topicId", getTopicId tag) ]
      ++ [ ("topicContent", markdownToHtmlSplice $ _content tag)
         , ("replyPerTopic", allReplyPerTopicSplice rs)
         , ("topicEditable", hasEditPermissionSplice user) ]

-- | @Splice@ is type synonium as @Splice m = HeistT m Template@
-- 
findTopicAuthorName :: Topic -> HeistT AppHandler T.Text
findTopicAuthorName topic = liftM _userDisplayName (findTopicAuthor topic)

findTopicAuthor :: Topic -> HeistT AppHandler User
findTopicAuthor topic = lift (findUser' topic)
                        where findUser' = findOneUser . _author

