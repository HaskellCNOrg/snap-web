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
import           Text.Digestive
import           Text.Digestive.Snap
import qualified Data.Text as T
import           Data.Maybe (fromJust)
import           Control.Monad.CatchIO (try, Exception(..))

import           Application

import           Controllers.User hiding (routes)
import           Views.TopicForm
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
                    Left e  -> liftIO $ print $ showUE e -- return to detail edit page with errors.
                    Right t -> redirect $ "/topic/" `BS.append` (pack' $ show objid)


-- | must be used after user login. 
-- 
topicVoToTopic :: TopicVo -> ObjectId -> T.Text -> MT.Topic
topicVoToTopic tv objid author = Topic 
                                 { _topicId = objid
                                 , _title = title tv
                                 , _content = content tv
                                 , _author = author
                                 }
                    
toTopicFormPage :: View T.Text -> AppHandler ()
toTopicFormPage = renderDfPage "topic-form"
            
------------------------------------------------------------------------------
                    
-- | what should do when 
--   1. no tid found, `when (isNothing tid) ...`
--   2. no result found
-- 
viewTopic :: AppHandler ()
viewTopic = do
  (Just tid) <- decodedParam' "topicid"
  t <- findOneTopic (read $ unpack' tid)
  case t of
    Nothing -> render "topic-detail"
    Just x  -> heistLocal (bindString "topicTitle" (_title x)) $ render "topic-detail"
                     

------------------------------------------------------------------------------
