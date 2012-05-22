{-# LANGUAGE OverloadedStrings #-}

module Views.TopicForm where

import Control.Applicative ((<$>), (<*>))
import Data.Text (Text)
import qualified Data.Text as T
import Text.Digestive

import Views.Validators
import Models.Topic
import Models.Utils


-- | VO is created because it is not quite easy to use single
--   model @Models.Topic.Topic@.
-- 
data TopicVo = TopicVo 
               { title   :: T.Text
               , content :: T.Text
               , topicId :: T.Text
               } deriving (Show)


-- | FIXME: Need a better design to get message from i18n snaplet.
-- 
topicForm :: Monad m => Form Text m TopicVo
topicForm = TopicVo
    <$> "title"    .: titleValidation (text Nothing)
    <*> "content"  .: contentValidation (text Nothing)
    <*> "tid"      .: text Nothing

-- | Render a form base on exists @Topic@ for editing.
-- 
topicEditForm :: Monad m => Topic -> Form Text m TopicVo
topicEditForm t = TopicVo
    <$> "title"    .: titleValidation (text $ Just $ _title t)
    <*> "content"  .: contentValidation (text $ Just $ _content t)
    <*> "tid"      .: checkForRequired "Fatal error happened.(tid is required)" (text . Just $ sToText (_topicId t))


-- | FIXME: is it possible doing in Monad?
-- 
titleValidation :: Monad m => Form Text m Text -> Form Text m Text
titleValidation = checkForMinLength 8 . checkForRequired "title is required"

contentValidation :: Monad m => Form Text m Text -> Form Text m Text
contentValidation = checkForMinLength 10 . checkForRequired "content is required"
