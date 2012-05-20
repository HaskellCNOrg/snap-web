{-# LANGUAGE OverloadedStrings, ExtendedDefaultRules #-}

module Views.TopicSplices
       ( topicSplices 
       , topicDetailSplices ) where

import           Control.Applicative ((<|>))
import           Control.Monad
import           Text.Templating.Heist
import           Control.Monad.Trans
import qualified Data.Text as T
import           Data.Time
import             Control.Arrow (second)

import           Application
import Models.Exception
import Models.Topic
import Models.Utils
import Views.ExceptionSplices
import Views.MarkdownSplices
import Views.Utils

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
    mapSplices renderTopicSimple t

------------------------------------------------------------------------------

-- | Splices used at Topic Detail page. 
--   Display either a topic or error msg.    
-- 
topicDetailSplices :: Either UserException Topic -> [(T.Text, Splice AppHandler)]
topicDetailSplices (Left l) = topicDetailSplices' Nothing (Just l)
topicDetailSplices (Right r) = topicDetailSplices' (Just r) Nothing


topicDetailSplices' :: Maybe Topic -> Maybe UserException -> [(T.Text, Splice AppHandler)]
topicDetailSplices' t e = [ ("ifTopic", renderTopic t)
                          , ("ifTopicError", renderUE e)]

------------------------------------------------------------------------------

-- | Single Topic to Splice
-- 
renderTopicSimple :: Topic -> Splice AppHandler
renderTopicSimple tag = runChildrenWithText 
      [ ("topicTitle", _title tag)
      , ("topicAuthor", _author tag)
      , ("oid", T.pack $ show (_topicId tag)) ]

renderTopic :: Maybe Topic -> Splice AppHandler
renderTopic Nothing = return []
renderTopic (Just tag) = runChildrenWith $
      map (second textSplice) [ ("topicTitle", _title tag)
                              , ("topicAuthor", _author tag)
                              , ("topicCreateAt", formatUTCTime $ _createAt tag)
                              , ("topicUpdateAt", formatUTCTime $ _updateAt tag)
                              , ("oid", sToText (_topicId tag)) ]
      ++ [ ("topicContent", markdownToHtmlSplice $ _content tag) ]
    
