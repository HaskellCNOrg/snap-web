{-# LANGUAGE OverloadedStrings, ExtendedDefaultRules #-}

module Controllers.Topic 
       ( routes
       , renderTopicDetailPage
       , redirectTopicDetailPage ) where

import           Control.Monad
import           Control.Monad.Trans
import           Control.Monad.CatchIO (throw, try)
import           Data.Maybe (fromJust, isNothing, fromMaybe)
import           Data.Time
import           Snap.Core
import qualified Snap.Core as Snap
import           Snap.Snaplet
import           Text.Digestive
import           Text.Digestive.Snap
import           Text.Templating.Heist
import qualified Data.ByteString as BS
import qualified Data.Text as T
import           Data.Bson (ObjectId)

import           Application
import           Controllers.User hiding (routes)
import           Controllers.Tag (saveTags)
import           Controllers.Home (redirect303)
import           Models.Utils
import           Views.TopicForm
import           Views.ReplyForm
import           Views.TopicSplices
import           Views.Utils
import           Models.Exception
import           Models.Topic
import qualified Models.User as User
import qualified Models.Tag as Tag


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

------------------------------------------------------------------------------

topicIdParam :: BS.ByteString
topicIdParam = "topicid"

redirectTopicDetailPage :: String -> AppHandler ()
redirectTopicDetailPage tid = redirect303 $ "/topic/" `BS.append` sToBS tid

------------------------------------------------------------------------------


tplTopicForm :: BS.ByteString
tplTopicForm = "topic-form"

tplTopicDetail :: BS.ByteString
tplTopicDetail = "topic-detail"


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
                  (Just uid') <- User.findCurrentUserId
                  tags <- doSaveTags tv
                  topic     <- try $ createNewTopic =<< liftIO (topicVoToNewTopic tv tags uid')
                  case topic of 
                    Left e  -> writeText $ showUE e 
                               -- FIXME:  return to detail edit page with errors.
                    Right t -> redirectTopicDetailPage (textToS $ getTopicId t)


toTopicFormPage :: View T.Text -> AppHandler ()
toTopicFormPage = renderDfPage tplTopicForm


------------------------------------------------------------------------------
                    
-- | topic detail viewer per topic id.
-- 
-- FIXME: change Model.findOneTopic to (Maybe ObjectId)
--        then use =? at mongoDB selector to see what is return value.
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
                                    tplTopicDetail
                                    view 
                                    (bindSplices (topicDetailSplices result))


------------------------------------------------------------------------------
                    
-- | Open 'Edit a topic' page.
-- 
editTopicH :: AppHandler ()
editTopicH = withAuthUser $ do
    tid <- decodedParamMaybe topicIdParam
    try (do when (isNothing tid) (toTopicDetailPage (Left $ UserException "tid not specifed"))
            findOneTopic (read $ bsToS $ fromJust tid)) >>= toEditTopicPageOr'

toEditTopicPageOr' :: Either UserException Topic -> AppHandler ()    
toEditTopicPageOr' = either (toTopicDetailPage . Left) toEditingPage
    where toEditingPage t = do
                            tags <- Tag.findSomeTags (fromMaybe [] $ _topicTags t)
                            --liftIO $ print tags
                            runForm "edit-topic-form" (topicEditForm t tags)
                            >>= (toTopicFormPage . fst)


-- | Save the edit to a topic.
--
saveTopicH :: AppHandler ()
saveTopicH = withAuthUser $ do
                 (view, result) <- runForm "edit-topic-form" topicForm
                 --liftIO $ print result            
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
  tags <- doSaveTags tv
  findTopic <- try (findOneTopic (textToObjectId $ topicId tv ))
  case findTopic of
    Left  l -> writeText $ showUE l -- FIXME:  1. return to detail edit page with errors.
                                    --         2. put findOneTopic and saveTopic in one try??
    Right r -> do
      result <- try $ saveTopic =<< liftIO (topicVoToTopic tv tags r)
      case result of 
        Left e  -> writeText $ showUE e  -- FIXME:  return to detail edit page with errors.

        Right t -> redirectTopicDetailPage (textToS $ getTopicId t)


------------------------------------------------------------------------------

-- | Save multiple Tags.
--
doSaveTags :: TopicVo -> AppHandler [Tag.Tag]
doSaveTags = saveTags . splitOnSpaceOrComma . topicTags


------------------------------------------------------------------------------

-- | Generate a new @Topic@ from @TopicVo@.
-- 
topicVoToNewTopic :: TopicVo -> [Tag.Tag] -> User.UserObjId -> IO Topic
topicVoToNewTopic tv tags author = do
    now <- getCurrentTime
    return Topic 
             { _topicId = Nothing
             , _title   = title tv
             , _content = content tv
             , _author  = author
             , _createAt = now
             , _updateAt = now
             , _topicTags = Tag.toTagIds tags
             }

-- | Populate change to a existing @Topic@ from topicVo
-- 
topicVoToTopic :: TopicVo -> [Tag.Tag] -> Topic -> IO Topic
topicVoToTopic tv tags topic = do
    now <- getCurrentTime
    return  topic 
             { _title    = title tv
             , _content  = content tv
             , _updateAt = now
             , _topicTags = Tag.toTagIds tags
             }

