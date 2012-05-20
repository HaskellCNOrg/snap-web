{-# LANGUAGE OverloadedStrings, ExtendedDefaultRules #-}

module Controllers.Topic 
       ( routes ) where

import           Data.Bson
import           Snap.Core
import           Snap.Snaplet
import           Snap.Snaplet.Heist
import           Snap.Snaplet.Auth
import           Text.Templating.Heist
import qualified Data.ByteString as BS
import           Control.Monad.Trans
import           Control.Monad
import           Text.Digestive
import           Text.Digestive.Snap
import qualified Data.Text as T
import           Data.Maybe (fromJust, isNothing)
import           Control.Monad.CatchIO (try)

import           Application

import           Controllers.User hiding (routes)
import           Views.TopicForm
import           Views.TopicSplices
import           Controllers.Utils
import           Models.Utils
import  Models.Topic 
import  Models.Exception 
import qualified Models.Topic as MT


------------------------------------------------------------------------------

routes :: [(BS.ByteString, Handler App App ())]
routes =  [ ("/topic",  createTopic)
          , ("/topic/:topicid", viewTopic)
          ]

------------------------------------------------------------------------------

-- | Renders the front page of the sample site.
-- 
createTopic :: AppHandler ()
createTopic = withAuthUser $ do
                             (view, result) <- runForm "create-topic-form" topicForm
                             case result of
                               Just topic -> createTopic' topic
                               Nothing    -> toTopicFormPage view
 
createTopic' :: TopicVo -> AppHandler ()
createTopic' tv = do 
                  objid     <- liftIO genObjectId
                  loginName <- fmap (userLogin . fromJust) $ with appAuth currentUser
                  topic     <- try $ MT.createNewTopic (topicVoToTopic tv objid loginName)
                  case topic of 
                    Left e  -> liftIO $ print $ showUE e 
                               -- ^ FIXME:  return to detail edit page with errors.
                    Right _ -> redirect $ "/topic/" `BS.append` (pack' $ show objid)


-- | must be used after user login. 
-- 
topicVoToTopic :: TopicVo -> ObjectId -> T.Text -> MT.Topic
topicVoToTopic tv objid author = Topic 
                                 { _topicId = objid
                                 , _title   = title tv
                                 , _content = content tv
                                 , _author  = author
                                 }
                    
toTopicFormPage :: View T.Text -> AppHandler ()
toTopicFormPage = renderDfPage "topic-form"
            
------------------------------------------------------------------------------
                    
-- | topic detail viewer.
-- 
viewTopic :: AppHandler ()
viewTopic = do
    tid <- decodedParamMaybe "topicid"
    when (isNothing tid) (toTopicDetailPage (Left $ UserException "tid not specifed"))
    toTopicDetailPage =<< try (findOneTopic (read $ unpack' $ fromJust tid))


toTopicDetailPage :: Either UserException Topic -> AppHandler ()    
toTopicDetailPage result = heistLocal (bindSplices (topicDetailSplices result)) $ render "topic-detail"


------------------------------------------------------------------------------
