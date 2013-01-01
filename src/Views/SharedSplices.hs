{-# LANGUAGE CPP                  #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE OverloadedStrings    #-}

module Views.SharedSplices where

import           Application
import           Data.Maybe                (fromMaybe)
import qualified Data.Text                 as T
import           Models.User
import           Snap
import           Snap.Snaplet.Environments
import           Snap.Snaplet.Heist
import           Text.Templating.Heist
import qualified Text.XmlHtml              as X


----------------------------------------------------------------------------


sharedSplices :: [(T.Text, SnapletSplice App App)]
sharedSplices = [ ("currentUser", currentUserSplice)
                , ("isCurrentUserAdmin", isCurrentUserAdminSplice)
                ]


----------------------------------------------------------------------------

-- | Current @User@ splice. Diff with Snaplet-Auth.loggedInUser which return @AuthUser@
--
currentUserSplice :: SnapletSplice App App
currentUserSplice = liftHandler findCurrentUser >>= liftHeist . textSplice . _userDisplayName

-- | Whether current user has admin role.
--
isCurrentUserAdminSplice :: SnapletSplice App App
isCurrentUserAdminSplice = do
    tf <- liftHandler isCurrentUserAdmin
    if tf then liftHeist runChildren else return []
