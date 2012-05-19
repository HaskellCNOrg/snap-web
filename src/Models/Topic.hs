
module Models.Topic where

import           Control.Applicative ((<$>), (<*>))
import           Control.Monad
import           Control.Monad.State
import           Snap.Core
import           Snap.Snaplet.Auth
import           Snap.Snaplet
import           Snap.Snaplet.Heist
import           Snap.Snaplet.I18N
import           Text.Digestive
import           Text.Digestive.Heist
import           Text.Digestive.Snap
import           Control.Monad.Trans
import           Text.Templating.Heist
import           Control.Monad.CatchIO (try, throw,  Exception(..))
import qualified Data.Text as T
import           Data.Bson

import           Application
import           Models.Exception
import           Models.Utils

-- | ? authorId is better to be objectId
--   ? another data type for digestive form?
-- 
data Topic = Topic
    { _topicId :: ObjectId
    , _title   :: T.Text
    , _content :: T.Text
    , _author  :: T.Text
    } deriving (Show)

------------------------------------------------------------------------------

-- | create a new topic. 
--   1). Assuming AuthUser exists cause upstream handler is working in @requrieUser@ handler.
-- 
createNewTopic :: Topic -> AppHandler Topic
createNewTopic topic = do
    newTopic <- return topic --with appAuth (saveTopic' topic')
    return newTopic        

------------------------------------------------------------------------------
