{-# LANGUAGE OverloadedStrings #-}

module Views.TopicForm where

import Control.Applicative ((<$>), (<*>))
import Data.Text (Text)
import qualified Data.Text as T
import Text.Digestive

import Views.Validators

-- | VO is created because it is not quite easy to use single
--   model @Models.Topic.Topic@.
-- 
data TopicVo = TopicVo 
               { title   :: T.Text
               , content :: T.Text
               } deriving (Show)


-- | FIXME: Need a better design to get message from i18n snaplet.
-- 
topicForm :: Monad m => Form Text m TopicVo
topicForm = TopicVo
    <$> "title"    .: titleValidation (text Nothing)
    <*> "content"  .: contentValidation (text Nothing)

-- | FIXME: is it possible doing in Monad?
-- 
titleValidation :: Monad m => Form Text m Text -> Form Text m Text
titleValidation = checkForMinLength 8 . checkForRequired "title is required"

contentValidation :: Monad m => Form Text m Text -> Form Text m Text
contentValidation = checkForMinLength 10 . checkForRequired "content is required"
