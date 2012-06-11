{-# LANGUAGE OverloadedStrings, ExtendedDefaultRules #-}

module Controllers.Topic 
       ( routes
       , renderTopicDetailPage
       , redirectTopicDetailPage ) where

import           Control.Monad
import           Control.Monad.Trans
import           Control.Monad.CatchIO (throw, try)
import           Data.Maybe (fromJust, isNothing)
import           Data.Time
import           Snap.Core
import qualified Snap.Core as Snap
import           Snap.Snaplet
import           Snap.Snaplet.Auth
import           Text.Digestive
import           Text.Digestive.Snap
import           Text.Templating.Heist
import qualified Data.ByteString as BS
import qualified Data.Text as T

import           Application
import           Controllers.User hiding (routes)
import           Models.Exception 
import           Models.Topic 
import           Models.Utils
import           Views.TopicForm
import           Views.ReplyForm
import           Views.TopicSplices
import           Views.Utils
import qualified Models.Topic as MT
import qualified Models.User as MU


------------------------------------------------------------------------------

-- | READ: the similarity is a little bit trick because of
--         1. want to separate create and save handler
--         2. action name (topic-form.tpl) is relative path'topic'
--            therefore it could be /topic or /topicput/topic base last URL.
-- 
routes :: [(BS.ByteString, Handler App App ())]
routes =  [ ("/topic",  createTopicH)                          -- save new topic
          , ("/topic/:topicid", Snap.method GET viewTopicH)    -- view a topic
          , ("/topicput/:topicid", Snap.method GET editTopicH) -- show detail for editing
          , ("/topicput/topic", Snap.method POST saveTopicH)   -- save editing changes. MAYBE: combine with topicput-GET.
          ]

topicIdParam :: BS.ByteString
topicIdParam = "topicid"

------------------------------------------------------------------------------


redirectTopicDetailPage :: String -> AppHandler ()
redirectTopicDetailPage tid = redirect $ "/topic/" `BS.append` sToBS tid

------------------------------------------------------------------------------

-- | Renders the front page of the sample site.
-- 
createTopicH :: AppHandler ()
createTopicH = withAuthUser $ do
                             (view, result) <- runForm "create-topic-form" topicForm
                             case result of
                               Just topic -> doCreateTopic' topic
                               Nothing    -> toTopicFormPage view
 
-- | Performance saving a new topic.
--   This is not public and assume done within login session.
-- 
doCreateTopic' :: TopicVo -> AppHandler ()
doCreateTopic' tv = do 
                  (Just uid') <- MU.findCurrentUserId
                  topic     <- try $ MT.createNewTopic =<< liftIO (topicVoToNewTopic tv uid')
                  case topic of 
                    Left e  -> writeText $ showUE e 
                               -- FIXME:  return to detail edit page with errors.
                    Right t -> redirectTopicDetailPage (textToS $ getTopicId t)


toTopicFormPage :: View T.Text -> AppHandler ()
toTopicFormPage = renderDfPage "topic-form"


------------------------------------------------------------------------------
                    
-- | topic detail viewer per topic id.
-- 
viewTopicH :: AppHandler ()
viewTopicH = do
    tid <- decodedParamMaybe topicIdParam
    try (do when (isNothing tid) (throw (UserException "Fatal: tid not specifed."))
            findOneTopic (read . bsToS . fromJust $ tid)) >>= toTopicDetailPage

toTopicDetailPage :: Either UserException Topic -> AppHandler ()    
toTopicDetailPage result = do (view, _) <- runReplyForm
                              renderTopicDetailPage result view

renderTopicDetailPage :: Either UserException Topic -> View T.Text -> AppHandler ()
renderTopicDetailPage result view = renderDfPageSplices 
                                    "topic-detail" 
                                    view 
                                    (bindSplices (topicDetailSplices result))


------------------------------------------------------------------------------
                    
-- | Edit a topic.
-- 
-- 
editTopicH :: AppHandler ()
editTopicH = withAuthUser $ do
    tid <- decodedParamMaybe topicIdParam
    try (do when (isNothing tid) (toTopicDetailPage (Left $ UserException "tid not specifed"))
            findOneTopic (read $ bsToS $ fromJust tid)) >>= toEditTopicPageOr'

toEditTopicPageOr' :: Either UserException Topic -> AppHandler ()    
toEditTopicPageOr' = either (toTopicDetailPage . Left) toEditingPage
    where toEditingPage t = runForm "edit-topic-form" (topicEditForm t) >>= (toTopicFormPage . fst)


saveTopicH :: AppHandler ()
saveTopicH = withAuthUser $ do
                 (view, result) <- runForm "edit-topic-form" topicForm
                 case result of
                   Just topic -> doUpdateTopic' topic
                   Nothing    -> toTopicFormPage view -- FIXME: bug..to form page should runForm first.
                                                      -- however this branch wont be hit bacuse `saveTopicH` 
                                                      -- only accept POST method

-- | Do update a existing Topic.
--   1. Fetch topic for updating, failed otherwise.
--   2. Save the change.
-- 
doUpdateTopic' :: TopicVo -> AppHandler ()
doUpdateTopic' tv = do
                  findTopic <- try (findOneTopic (read $ textToS $ topicId tv ))
                  case findTopic of
                      Left  l -> writeText $ showUE l -- FIXME:  1. return to detail edit page with errors.
                                                      --         2. put findOneTopic and saveTopic in one try??
                      Right r -> do
                                   result <- try $ MT.saveTopic =<< liftIO (topicVoToTopic tv r)
                                   case result of 
                                         Left e  -> writeText $ showUE e 
                                                     -- FIXME:  return to detail edit page with errors.
                                         Right t -> redirectTopicDetailPage (textToS $ getTopicId t)

------------------------------------------------------------------------------

-- | Generate a new @Topic@ from @TopicVo@.
-- 
topicVoToNewTopic :: TopicVo -> MU.UserObjId -> IO MT.Topic
topicVoToNewTopic tv author = do
    now <- getCurrentTime
    return  Topic 
             { _topicId = Nothing
             , _title   = title tv
             , _content = content tv
             , _author  = author
             , _createAt = now
             , _updateAt = now              
             }

-- | Populate change to a existing @Topic@ from topicVo
--   FIXME: may update by other users, e.g. Administrator user.
-- 
topicVoToTopic :: TopicVo -> MT.Topic -> IO MT.Topic
topicVoToTopic tv topic = do
    now <- getCurrentTime
    return  topic 
             { _title    = title tv
             , _content  = content tv
             , _updateAt = now              
             }
