{-# LANGUAGE OverloadedStrings, ExtendedDefaultRules #-}

module Controllers.Home where

import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import           Control.Monad.Trans
import           Control.Monad.State
import           Snap.Core
import           Snap.Snaplet.Auth
import           Snap.Snaplet
import           Snap.Snaplet.Heist
import           Snap.Snaplet.Session
import           Text.Templating.Heist
import           Text.XmlHtml hiding (render)

import           Application
import           Controllers.Utils

------------------------------------------------------------------------------

-- | Renders the front page of the sample site.
--
-- The 'ifTop' is required to limit this to the top of a route.
-- Otherwise, the way the route table is currently set up, this action
-- would be given every request.
index :: Handler App App ()
index = ifTop $ render "index"

