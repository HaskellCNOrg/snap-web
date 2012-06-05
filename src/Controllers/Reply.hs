{-# LANGUAGE OverloadedStrings, ExtendedDefaultRules #-}

module Controllers.Reply 
       ( routes ) where

import           Control.Monad.Trans
import           Control.Monad.CatchIO (try)
import           Data.Time
import           Snap.Core
import           Snap.Snaplet
import           Text.Digestive
import           Text.Digestive.Snap
import qualified Data.ByteString as BS
import qualified Data.Text as T
import qualified Snap.Core as Snap

import           Application
import           Controllers.Topic hiding (routes)
import           Controllers.User (withAuthUser)
import           Models.Utils
import           Views.ReplyForm
import qualified Models.Topic as MT
import qualified Models.Reply as MR
import qualified Models.User as MU


------------------------------------------------------------------------------

routes :: [(BS.ByteString, Handler App App ())]
routes =  [ ("/reply", Snap.method POST replyToTopicH)
          ]


------------------------------------------------------------------------------

-- | Handler for saving reply to a topic.
-- 
replyToTopicH :: AppHandler ()
replyToTopicH = withAuthUser $ 
               do (view, result) <- runForm "reply-to-topic-form" replyForm
                  case result of
                    Just reply -> replyVoToReply reply
                                  >>= MR.createReplyToTopic 
                                  >> redirectTopicDetailPage ( textToS $ replyToTopicId reply)  
                                     -- shall be redirect otherwise URL doest change.
                    Nothing    -> toTopicDetailAfterReply view (fieldInputText "replyToTopicId" view)

toTopicDetailAfterReply :: View T.Text -> T.Text ->AppHandler ()
toTopicDetailAfterReply view tid = do re <- try (findOneTopic' tid)
                                      renderTopicDetailPage re view

findOneTopic' :: T.Text -> AppHandler MT.Topic
findOneTopic' = MT.findOneTopic . read . textToS


------------------------------------------------------------------------------
--

replyVoToReply :: ReplyVo -> AppHandler MR.Reply
replyVoToReply vo = do
    now <- liftIO getCurrentTime
    (Just uid') <- MU.findCurrentUserId
    return $ MR.Reply Nothing (textToObjectId $ replyToTopicId vo) 
                      Nothing (replyContent vo)
                      uid' now
