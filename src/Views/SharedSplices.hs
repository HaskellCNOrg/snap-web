{-# LANGUAGE OverloadedStrings, ExtendedDefaultRules #-}

module Views.SharedSplices where

import           Snap.Snaplet.Heist
import           Text.Templating.Heist
import qualified Data.Text as T


import           Application
import           Models.User

sharedSplices :: [(T.Text, SnapletSplice App App)]
sharedSplices = [ ("currentUser", currentUserSplice) 
                ]


----------------------------------------------------------------------------

-- | Current @User@ splice. Diff with Snaplet-Auth.loggedInUser which return @AuthUser@
-- 
currentUserSplice :: SnapletSplice App App
currentUserSplice = liftHandler findCurrentUser >>= liftHeist . textSplice . _userDisplayName

