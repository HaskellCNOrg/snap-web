{-# LANGUAGE OverloadedStrings, ExtendedDefaultRules #-}

module Controllers.Topic 
       ( routes ) where

import           Data.Bson
import           Control.Monad
import           Snap.Core
import           Snap.Snaplet
import           Snap.Snaplet.Heist
import           Snap.Snaplet.Auth
import           Text.Templating.Heist
import qualified Data.ByteString as BS
import           Control.Monad.Trans
import           Control.Applicative ((<$>), (<*>))
import           Text.Digestive
import           Text.Digestive.Heist
import           Text.Digestive.Snap
import qualified Data.Text as T
import           Data.Maybe (fromJust)

import           Application

import           Controllers.User hiding (routes)
import           Views.TopicForm
import           Controllers.Utils
import           Models.Utils
import  Models.Topic 
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
 
toTopicFormPage :: View T.Text -> AppHandler ()
toTopicFormPage = renderDfPage "topic-form"
            
createTopic' :: TopicVo -> AppHandler ()
createTopic' tv = do 
                  objid <- liftIO genObjectId
                  loginName <- fmap (userLogin . fromJust) $ with appAuth currentUser
                  topic <- MT.createNewTopic $ topicVoToTopic tv objid loginName
                  redirect . textToBs $ "/topic/" `T.append` (_author topic)


-- | must be used after user login. 
-- 
topicVoToTopic :: TopicVo -> ObjectId -> T.Text -> MT.Topic
topicVoToTopic tv objid author = Topic 
                                 { _topicId = objid
                                 , _title = title tv
                                 , _content = content tv
                                 , _author = author
                                 }
                    
                    
viewTopic :: AppHandler ()
viewTopic = render "topic-detail"
                     

