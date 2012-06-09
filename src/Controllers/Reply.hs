{-# LANGUAGE OverloadedStrings, ExtendedDefaultRules #-}

module Controllers.Reply 
       ( routes ) where

import           Control.Monad.CatchIO (try)
import           Control.Monad.Trans
import           Data.Time
import           Snap.Core
import           Snap.Snaplet
import           Snap.Snaplet.Heist
import           Text.Digestive
import           Text.Templating.Heist
import qualified Data.ByteString as BS
import qualified Data.Text as T
import qualified Snap.Core as Snap

import           Application
import           Controllers.Topic hiding (routes)
import           Controllers.User (withAuthUser)
import           Models.Utils
import           Views.ReplyForm
import           Views.TopicSplices
import           Views.Utils
import qualified Models.Reply as MR
import qualified Models.Topic as MT
import qualified Models.User as MU


------------------------------------------------------------------------------

routes :: [(BS.ByteString, Handler App App ())]
routes =  [ ("/reply", Snap.method POST replyToTopicH)
          , ("/topic/:topicid/:replyid/reply", replyToReplyH)
          , ("/topic/:topicid/:replyid/delete", replyDeleteH)
          ]

------------------------------------------------------------------------------

topicIdP :: BS.ByteString
topicIdP = "topicid"

replyIdP :: BS.ByteString
replyIdP = "replyid"

------------------------------------------------------------------------------

tplReplyToReplyForm :: BS.ByteString
tplReplyToReplyForm = "reply-to-reply-form"

tplReplyToReplyDetail :: BS.ByteString
tplReplyToReplyDetail = "reply-to-reply-detail"


------------------------------------------------------------------------------

-- | Handler for saving reply to a topic.
-- 
replyToTopicH :: AppHandler ()
replyToTopicH = withAuthUser $ 
               do (view, result) <- runReplyForm
                  case result of
                    Just reply -> replyVoToReply reply
                                  >>= MR.createReplyToTopic 
                                  >> redirectTopicDetailPage (textToS $ replyToTopicId reply)  
                                     -- shall be redirect otherwise URL doest change.
                    Nothing    -> toTopicDetailAfterReply view (fieldInputText "replyToTopicId" view)
                                   -- FIXME:  URL doest change.

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
    case result of
      Just req -> do 
                  reply <- MR.createReplyToTopic =<< replyVoToReply req
                  heistLocal (bindSplice "areply" $ replySplice reply) $ render tplReplyToReplyDetail
      Nothing  -> renderDfPageSplices tplReplyToReplyForm view $
                                      bindSplices [ ("topicid", textSplice tid)
                                                  , ("replyid", textSplice rid) ]


------------------------------------------------------------------------------

-- deleteReply
-- 
replyDeleteH :: AppHandler ()
replyDeleteH = withAuthUser $ do
    tid <- decodedParam topicIdP
    maybeRid <- decodedParamTextMaybe replyIdP
    case maybeRid of
      Nothing -> redirectTopicDetailPage (bsToS tid)
      Just rid  -> MR.deleteReply (textToObjectId rid)
                   >> redirectTopicDetailPage (bsToS tid)

------------------------------------------------------------------------------
--

replyVoToReply :: ReplyVo -> AppHandler MR.Reply
replyVoToReply vo = do
    now <- liftIO getCurrentTime
    (Just uid') <- MU.findCurrentUserId
    return $ MR.Reply Nothing 
                      (textToObjectId $ replyToTopicId vo) 
                      (trReplyId' $ replyToReplyId vo) 
                      (replyContent vo)
                      uid' now
    where trReplyId' "" = Nothing
          trReplyId' xs = Just $ textToObjectId xs 
