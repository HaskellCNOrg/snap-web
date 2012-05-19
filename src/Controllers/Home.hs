{-# LANGUAGE OverloadedStrings, ExtendedDefaultRules #-}

module Controllers.Home where

import           Snap.Core
import           Snap.Snaplet
import           Snap.Snaplet.Heist

import           Application

------------------------------------------------------------------------------

-- | Renders the front page of the sample site.
--
-- The 'ifTop' is required to limit this to the top of a route.
-- Otherwise, the way the route table is currently set up, this action
-- would be given every request.
index :: Handler App App ()
index = ifTop $ render "index"

redirectToHome :: Handler App App ()
redirectToHome = redirect "/"
