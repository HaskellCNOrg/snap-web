{-# LANGUAGE OverloadedStrings, ExtendedDefaultRules #-}

module Views.TagSplices where

import           Control.Arrow (second)
import           Control.Monad.Trans
--import           Control.Monad (liftM)
import Data.Maybe (fromMaybe)
--import           Data.Function (on)
import           Text.Templating.Heist
import           Data.List
import           Data.Bson (ObjectId)
import qualified Data.Map as MP
import qualified Data.Text as T

import Application
import Models.Tag
import Models.Utils

------------------------------------------------------------------------------

-- | Splice of listing multiple tags
--
topicTagSplice :: Maybe [ObjectId] -> Splice AppHandler
topicTagSplice ids =
    lift (findSomeTags $ fromMaybe [] ids)
    >>= mapSplices tagSplice


tagSplice :: Tag -> Splice AppHandler
tagSplice = runChildrenWithText . tagSpliceImpl


tagSpliceImpl :: Tag -> [(T.Text, T.Text)]
tagSpliceImpl (Tag id name _) = [ ("tagId", objectIdToText id) 
                                , ("tagName", name) ]
