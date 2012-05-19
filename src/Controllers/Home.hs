{-# LANGUAGE OverloadedStrings, ExtendedDefaultRules #-}

module Controllers.Home where

import           Snap.Core
import           Snap.Snaplet
import           Snap.Snaplet.Heist
import           Text.Templating.Heist
import           Control.Monad.Trans
--import           Snap.Snaplet.Auth
--import           Snap.Snaplet.MongoDB
--import           Snap.Snaplet.Auth

import           Application

import Views.TopicSplices

------------------------------------------------------------------------------

-- | Renders the front page of the sample site.
-- 
index :: Handler App App ()
index = ifTop $ do
        heistLocal (bindSplices topicSplices) $ render "index"

redirectToHome :: Handler App App ()
redirectToHome = redirect "/"
