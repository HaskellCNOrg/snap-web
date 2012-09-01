{-# LANGUAGE OverloadedStrings #-}

module Views.TopicForm where

import Control.Applicative ((<$>), (<*>))
import Data.Text (Text)
import qualified Data.Text as T
import Text.Digestive
import Text.Digestive.FormExt

import Models.Topic
import Models.Tag
import Models.Utils


-- | VO is created because it is not quite easy to use single
--   model @Models.Topic.Topic@.
-- 
data TopicVo = TopicVo 
               { title   :: T.Text
               , content :: T.Text
               , topicId :: T.Text
               , topicTags :: T.Text
               } deriving (Show)


-- | FIXME: Need a better design to get message from i18n snaplet.
-- 
topicForm :: Monad m => Form Text m TopicVo
topicForm = TopicVo
    <$> "title"    .: titleValidation (text Nothing)
    <*> "content"  .: contentValidation (text Nothing)
    <*> "tid"      .: text Nothing
    <*> "tags"     .: text Nothing

-- | Render a form base on exists @Topic@ for editing.
--
-- 
topicEditForm :: Monad m => Topic -> [Tag] -> Form Text m TopicVo
topicEditForm t tags = TopicVo
    <$> "title"    .: titleValidation (text $ Just $ _title t)
    <*> "content"  .: contentValidation (text $ Just $ _content t)
    <*> "tid"      .: checkRequired "Fatal error happened.(tid is required)" (text $ fmap sToText (_topicId t))
    <*> "tags"     .: tagsToText tags

-- | combinate tag names to display.
-- 
tagsToText :: Monad m => [Tag] -> Form Text m Text
tagsToText = text . Just . T.intercalate " " . map _tagName


-- | Topic Title Validation. (Required + minlength 5)
-- 
titleValidation :: Monad m => Form Text m Text -> Form Text m Text
titleValidation = checkMinLength 5 . checkRequired "title is required"

-- | Topic Content Validation. (Required + minlength 10)
-- 
contentValidation :: Monad m => Form Text m Text -> Form Text m Text
contentValidation = checkMinLength 10 . checkRequired "content is required"

