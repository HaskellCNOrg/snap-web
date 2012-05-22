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
import qualified Snap.Core as Snap
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
routes =  [ ("/topic",  createTopicH)  -- save new topic
          , ("/topic/:topicid", Snap.method GET viewTopicH)    -- view a topic
          , ("/topicput/:topicid", Snap.method GET editTopicH) -- show detail for editing
          , ("/topicput/topic", Snap.method POST saveTopicH)  -- save editing changes.
          ]

topicIdParam :: BS.ByteString
topicIdParam = "topicid"

------------------------------------------------------------------------------

-- | Renders the front page of the sample site.
-- 
createTopicH :: AppHandler ()
createTopicH = withAuthUser $ do
                             (view, result) <- runForm "create-topic-form" topicForm
                             case result of
                               Just topic -> doCreateTopic' topic
                               Nothing    -> toTopicFormPage view
 
-- | Save a new topic
doCreateTopic' :: TopicVo -> AppHandler ()
doCreateTopic' tv = do 
                  loginName <- fmap (userLogin . fromJust) $ with appAuth currentUser
                  topic     <- try $ MT.createNewTopic =<< liftIO (topicVoToNewTopic tv loginName)
                  case topic of 
                    Left e  -> writeText $ showUE e 
                               -- FIXME:  return to detail edit page with errors.
                    Right t -> redirectTopicDetailPage (show $ _topicId t)



                   
toTopicFormPage :: View T.Text -> AppHandler ()
toTopicFormPage = renderDfPage "topic-form"

redirectTopicDetailPage :: String -> AppHandler ()
redirectTopicDetailPage tid = redirect $ "/topic/" `BS.append` sToBS tid

------------------------------------------------------------------------------
                    
-- | topic detail viewer per topic id.
-- 
viewTopicH :: AppHandler ()
viewTopicH = do
    tid <- decodedParamMaybe topicIdParam
    -- FIXME: Actually this when will not terminate the computation thus will result in error forwards.
    --        Use `error` for work around.
    --        how to reproduce: use a incorrect param, e.g. "testp"
    --   when (isNothing tid) (toTopicDetailPage (Left $ UserException "tid not specifed"))
    when (isNothing tid) (error "tid not specifed")
    toTopicDetailPage =<< try (findOneTopic (read $ bsToS $ fromJust tid))

toTopicDetailPage :: Either UserException Topic -> AppHandler ()    
toTopicDetailPage result = heistLocal (bindSplices (topicDetailSplices result)) $ render "topic-detail"


------------------------------------------------------------------------------
                    
-- | Edit a topic.
-- 
-- TODO:
--  1. same problem with viewTopicH
--  2. tid must be valid objectId in order to be parsed. otherwise error.
--     reproduce: access to /topicput/testing (read: no parse)
editTopicH :: AppHandler ()
editTopicH = withAuthUser $ do
    tid <- decodedParamMaybe topicIdParam
    when (isNothing tid) (toTopicDetailPage (Left $ UserException "tid not specifed"))
    toEditTopicPageOr' =<< try (findOneTopic (read $ bsToS $ fromJust tid))

toEditTopicPageOr' :: Either UserException Topic -> AppHandler ()    
toEditTopicPageOr' result = either (toTopicDetailPage . Left) toEditingPage result
    where toEditingPage t = runForm "edit-topic-form" (topicEditForm t) >>= (toTopicFormPage . fst)


saveTopicH :: AppHandler ()
saveTopicH = withAuthUser $ do
                 (view, result) <- runForm "edit-topic-form" topicForm
                 case result of
                   Just topic -> doUpdateTopic' topic
                   Nothing    -> toTopicFormPage view

-- | Do update a existing Topic.
--   1. Fetch topic for updating, failed otherwise.
--   2. Save the change.
-- 
doUpdateTopic' :: TopicVo -> AppHandler ()
doUpdateTopic' tv = do
                  findTopic <- try (findOneTopic (read $ textToS $ topicId tv ))
                  case findTopic of
                      Left  l -> writeText $ showUE l
                      Right r -> do
                                   result <- try $ MT.saveTopic =<< liftIO (topicVoToTopic tv r)
                                   case result of 
                                         Left e  -> writeText $ showUE e 
                                                     -- FIXME:  return to detail edit page with errors.
                                         Right t -> redirectTopicDetailPage (show $ _topicId t)

------------------------------------------------------------------------------

-- | Generate a new @Topic@ from @TopicVo@.
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

-- | Populate change to a existing @Topic@ from topicVo
-- 
topicVoToTopic :: TopicVo -> MT.Topic -> IO MT.Topic
topicVoToTopic tv topic = do
    now <- getCurrentTime
    return  topic 
             { _title    = title tv
             , _content  = content tv
             , _updateAt = now              
             }
