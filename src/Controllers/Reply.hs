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
import           Models.Topic 
import           Models.Utils
import           Views.TopicForm
import           Views.ReplyForm
import           Views.TopicSplices
import           Views.Utils
import qualified Models.Topic as MT


------------------------------------------------------------------------------

routes :: [(BS.ByteString, Handler App App ())]
routes =  [ ("/reply", Snap.method POST replyToTopic)
          ]


------------------------------------------------------------------------------

-- | Renders the front page of the sample site.
-- 
replyToTopic :: AppHandler ()
replyToTopic = withAuthUser $ 
               do (view, result) <- runForm "reply-to-topic-form" replyForm
                  case result of
                    Just reply -> redirectToHome      -- FIXME: save reply
                    Nothing    -> do re <- try (findOneTopic' (fieldInputText "replyToTopicId" view))
                                     renderTopicDetailPage re view

findOneTopic' :: T.Text -> AppHandler Topic
findOneTopic' = findOneTopic . read . textToS
