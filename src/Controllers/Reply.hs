{-# LANGUAGE OverloadedStrings, ExtendedDefaultRules #-}

module Controllers.Reply 
       ( routes ) where

import           Control.Monad
import           Control.Monad.CatchIO (try,throw,Exception(..))
import           Control.Monad.Trans
import           Data.Maybe (fromJust, isNothing)
import           Data.Time
import           Snap.Core
import qualified Snap.Core as Snap
import           Snap.Snaplet
import           Snap.Core
import           Snap.Snaplet
import           Snap.Snaplet.Auth
import           Snap.Snaplet.Heist
import           Text.Digestive
import           Text.Digestive.Snap
import           Text.Templating.Heist
import qualified Data.ByteString as BS
import qualified Data.Text as T

import           Application
import           Controllers.Topic hiding (routes)
import           Controllers.Home (redirectToHome)
import           Controllers.User (withAuthUser)
import           Models.Exception 
--import           Models.Topic 
import           Models.Utils
import           Views.TopicForm
import           Views.ReplyForm
import           Views.TopicSplices
import           Views.Utils
import qualified Models.Topic as MT
import qualified Models.Reply as MR


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
                    Just reply -> liftIO (replyVoToReply reply) 
                                  >>= MR.createReplyToTopic 
                                  >> toTopicDetailAfterReply view (replyToTopicId reply)
                    Nothing    -> toTopicDetailAfterReply view (fieldInputText "replyToTopicId" view)

toTopicDetailAfterReply :: View T.Text -> T.Text ->AppHandler ()
toTopicDetailAfterReply view tid = do re <- try (findOneTopic' tid)
                                      renderTopicDetailPage re view

findOneTopic' :: T.Text -> AppHandler MT.Topic
findOneTopic' = MT.findOneTopic . read . textToS


------------------------------------------------------------------------------
--

replyVoToReply :: ReplyVo -> IO MR.Reply
replyVoToReply vo = do
    now <- getCurrentTime
    return $ MR.Reply Nothing (textToObjectId $ replyToTopicId vo) 
                      Nothing (replyContent vo)
                      "dummy author" now
