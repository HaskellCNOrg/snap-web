{-# LANGUAGE OverloadedStrings #-}

module Views.ReplyForm where

import Control.Applicative ((<$>), (<*>))
import Data.Text (Text)
import qualified Data.Text as T
import Snap
import Text.Digestive
import Text.Digestive.Snap

import Views.Validators


-- | 
-- 
data ReplyVo = ReplyVo 
               { replyToTopicId :: T.Text
               , replyContent :: T.Text
               } deriving (Show)
-- |
-- 
runReplyForm :: MonadSnap m => m (View Text, Maybe ReplyVo)
runReplyForm = runForm "reply-to-topic-form" replyForm

-- |
--  
replyForm :: Monad m => Form Text m ReplyVo
replyForm = ReplyVo
    <$> "replyToTopicId"  .: checkRequired "replyToTopicId is required" (text Nothing)
    <*> "content"  .: contentValidation (text Nothing)


contentValidation :: Monad m => Form Text m Text -> Form Text m Text
contentValidation = checkMinLength 6 . checkRequired "Reply content can not be empty."
