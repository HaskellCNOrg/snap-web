{-# LANGUAGE OverloadedStrings, ExtendedDefaultRules #-}

module Views.SharedSplices where

import           Snap.Snaplet.Auth
import           Text.Templating.Heist
import qualified Data.Text as T

import           Snap.Snaplet
import           Snap.Snaplet.Heist


import           Application
import Models.Exception
import Models.User
import Views.Utils
import Views.Types

sharedSplices :: [(T.Text, SnapletSplice App App)]
sharedSplices = [ ("currentUser", currentUserSplice) 
                ]


----------------------------------------------------------------------------

-- | Current @User@ splice. Diff with Snaplet-Auth.loggedInUser which return @AuthUser@
-- 
currentUserSplice :: SnapletSplice App App
currentUserSplice = do
    liftHandler findCurrentUser >>= liftHeist . (textSplice . _userDisplayName)
     
