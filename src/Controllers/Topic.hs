{-# LANGUAGE OverloadedStrings, ExtendedDefaultRules #-}

module Controllers.Topic 
       ( routes ) where

import           Control.Monad
import           Control.Monad.CatchIO (try)
import           Control.Monad.Trans
import           Data.Bson
import           Data.Maybe (fromJust, isNothing)
import           Data.Time
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
import           Controllers.User hiding (routes)
import           Controllers.Utils
import           Models.Exception 
import           Models.Topic 
import           Models.Utils
import           Views.TopicForm
import           Views.TopicSplices
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
                  loginName <- fmap (userLogin . fromJust) $ with appAuth currentUser
                  topic     <- try $ MT.createNewTopic =<< liftIO (topicVoToNewTopic tv loginName)
                  case topic of 
                    Left e  -> liftIO $ print $ showUE e 
                               -- FIXME:  return to detail edit page with errors.
                    Right t -> redirect $ "/topic/" `BS.append` sToBS (show $ _topicId t)


-- | must be used after user login. 
-- 
topicVoToNewTopic :: TopicVo -> T.Text -> IO MT.Topic
topicVoToNewTopic tv author = do
    objid <- genObjectId
    now <- getCurrentTime
    return  Topic 
             { _topicId = objid
             , _title   = title tv
             , _content = content tv
             , _author  = author
             , _createAt = now
             , _updateAt = now              
             }
                    
toTopicFormPage :: View T.Text -> AppHandler ()
toTopicFormPage = renderDfPage "topic-form"
            
------------------------------------------------------------------------------
                    
-- | topic detail viewer per topic id.
-- 
viewTopic :: AppHandler ()
viewTopic = do
    tid <- decodedParamMaybe "topicid"
    when (isNothing tid) (toTopicDetailPage (Left $ UserException "tid not specifed"))
    toTopicDetailPage =<< try (findOneTopic (read $ bsToS $ fromJust tid))

toTopicDetailPage :: Either UserException Topic -> AppHandler ()    
toTopicDetailPage result = heistLocal (bindSplices (topicDetailSplices result)) $ render "topic-detail"


------------------------------------------------------------------------------
