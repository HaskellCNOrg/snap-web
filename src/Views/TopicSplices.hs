{-# LANGUAGE OverloadedStrings, ExtendedDefaultRules #-}

module Views.TopicSplices
       ( topicSplices 
       , topicDetailSplices ) where

import           Text.Templating.Heist
import           Control.Monad.Trans
import qualified Data.Text as T

import           Application

import Models.Topic
import Models.Exception
import Views.ExceptionSplices

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
renderTopicSimple tag = do
    runChildrenWithText 
      [ ("topicTitle", _title tag)
      , ("oid", T.pack $ show (_topicId tag)) ]

renderTopic :: Maybe Topic -> Splice AppHandler
renderTopic Nothing = return []
renderTopic (Just tag) = do
    runChildrenWithText 
      [ ("topicTitle", _title tag)
      , ("topicAuthor", _author tag)
      , ("oid", T.pack $ show (_topicId tag)) ]

