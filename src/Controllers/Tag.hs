{-# LANGUAGE OverloadedStrings, ExtendedDefaultRules #-}

module Controllers.Tag
       ( routes
) where

import           Data.Aeson
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
import           Models.Utils
import           Views.Utils
import           Models.Tag
--import qualified Models.Tag as MT
import qualified Models.User as MU


------------------------------------------------------------------------------

-- | READ: the similarity is a little bit trick because of
--         1. want to separate create and save handler
--         2. action name (topic-form.tpl) is relative path'topic'
--            therefore it could be /topic or /topicput/topic base last URL.
-- 
routes :: [(BS.ByteString, Handler App App ())]
routes =  [ ("/tags",  Snap.method GET getTags)
          ]

contenTypeJSON :: BS.ByteString
contenTypeJSON = "application/json"

setJSONContentType :: Response -> Response
setJSONContentType = setContentType contenTypeJSON

------------------------------------------------------------------------------

                    
-- | topic detail viewer per topic id.
-- 
-- FIXME: 1. if content-tye is not json, return empty
-- 
getTags :: AppHandler ()
getTags = do
    tags <- findAllTags
    modifyResponse setJSONContentType
    writeLBS $ encode $ toJSON tags


----------------------------------------
-- To be JSON 

instance ToJSON Tag where
  toJSON (Tag id name _) = object [ "id" .= name
                                  , "name" .= name
                                  ]
