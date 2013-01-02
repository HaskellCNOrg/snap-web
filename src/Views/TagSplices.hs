{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE OverloadedStrings    #-}

module Views.TagSplices where

import           Control.Monad.Trans
import           Data.Bson             (ObjectId)
import           Data.Maybe            (fromMaybe)
import qualified Data.Text             as T
import           Text.Templating.Heist

import           Application
import           Models.Tag
import           Models.Utils

------------------------------------------------------------------------------

-- | Splice of listing multiple tags
--
topicTagSplice :: Maybe [ObjectId] -> Splice AppHandler
topicTagSplice ids =
    lift (findSomeTags $ fromMaybe [] ids)
    -- >>= mapSplices tagSplice
    >>= tagsSplice

tagSplice :: Tag -> Splice AppHandler
tagSplice = runChildrenWithText . tagSpliceImpl


tagSpliceImpl :: Tag -> [(T.Text, T.Text)]
tagSpliceImpl (Tag tid name _) = [ ("tagId", objectIdToText tid)
                                , ("tagName", name) ]

tagsSplice :: [Tag] -> Splice AppHandler
tagsSplice = mapSplices tagSplice
