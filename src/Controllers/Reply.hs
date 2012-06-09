{-# LANGUAGE OverloadedStrings, ExtendedDefaultRules #-}

module Controllers.Reply 
       ( routes ) where

import           Control.Monad.Trans
import           Control.Monad.CatchIO (try)
import           Data.Time
import           Snap.Core
import           Snap.Snaplet
import           Text.Digestive
import           Text.Templating.Heist
import           Text.Digestive.Snap
import qualified Data.ByteString as BS
import qualified Data.Text as T
import qualified Snap.Core as Snap

import           Application
import           Controllers.Topic hiding (routes)
import           Controllers.User (withAuthUser)
import           Models.Utils
import           Views.ReplyForm
import           Views.Utils
import qualified Models.Topic as MT
import qualified Models.Reply as MR
import qualified Models.User as MU


------------------------------------------------------------------------------

routes :: [(BS.ByteString, Handler App App ())]
routes =  [ ("/reply", Snap.method POST replyToTopicH)
          , ("/replytoreply", replyToReplyH)
          ]

------------------------------------------------------------------------------

topicIdP :: BS.ByteString
topicIdP = "topicId"

replyIdP :: BS.ByteString
replyIdP = "replyId"

------------------------------------------------------------------------------

tplReplyToReplyForm :: BS.ByteString
tplReplyToReplyForm = "reply-to-reply-form"

------------------------------------------------------------------------------

-- | Handler for saving reply to a topic.
-- 
replyToTopicH :: AppHandler ()
replyToTopicH = withAuthUser $ 
               do (view, result) <- runReplyForm
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

-- | Handler for reply to a reply.
-- 
replyToReplyH :: AppHandler ()
replyToReplyH = withAuthUser $ do 
    tid <- decodedParamText topicIdP
    rid <- decodedParamText replyIdP
    (view, result) <- runReplyToRelpyForm
    --liftIO $ print (view, result)
    case result of
      Just req -> writeBS "replyToReplyH"
      Nothing  -> renderDfPageSplices tplReplyToReplyForm view $
                                      bindSplices [ ("topicId", textSplice tid)
                                                  , ("replyId", textSplice rid) ]


------------------------------------------------------------------------------
--

replyVoToReply :: ReplyVo -> AppHandler MR.Reply
replyVoToReply vo = do
    now <- liftIO getCurrentTime
    (Just uid') <- MU.findCurrentUserId
    return $ MR.Reply Nothing (textToObjectId $ replyToTopicId vo) 
                      Nothing (replyContent vo)
                      uid' now
