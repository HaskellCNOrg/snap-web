{-# LANGUAGE OverloadedStrings, ExtendedDefaultRules #-}

module Controllers.Home where

import           Control.Monad.Trans
import           Snap.Core
import           Snap.Snaplet
import           Snap.Snaplet.Heist
import           Text.Templating.Heist

import           Application

import Views.TopicSplices
import Views.Utils

------------------------------------------------------------------------------

-- | Renders the front page of the sample site.
-- 
index :: Handler App App ()
index = ifTop $ do
    decodedParamNum "pagenum" >>= (liftIO . print)
    heistLocal (bindSplices topicSplices) $ render "index"

redirectToHome :: Handler App App ()
redirectToHome = redirect303 "/"

redirect303 url = redirect' url 303
