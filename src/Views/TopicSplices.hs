{-# LANGUAGE OverloadedStrings, ExtendedDefaultRules #-}

module Views.TopicSplices
       ( topicSplices ) where

import           Text.Templating.Heist
import           Control.Monad.Trans
import qualified Data.Text as T

import           Application

import Models.Topic

------------------------------------------------------------------------------
                    
-- | db.Topic.findAll
-- 

topicSplices :: [(T.Text, Splice AppHandler)]
topicSplices = [("allTopics", tagsListSplice)]

tagsListSplice :: Splice AppHandler
tagsListSplice = do
    t <- lift findAllTopic  
    mapSplices renderTag t

renderTag:: Topic -> Splice AppHandler
renderTag tag = do
    runChildrenWithText 
      [ ("topicTitle", _title tag)
      , ("oid", T.pack $ show (_topicId tag)) ]
