{-# LANGUAGE CPP                  #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE OverloadedStrings    #-}

module Views.SharedSplices where

import           Application
import qualified Data.Text          as T
import qualified Heist.Interpreted  as I
import           Models.User
import           Snap
import           Snap.Snaplet.Heist

----------------------------------------------------------------------------

sharedSplices :: [(T.Text, SnapletISplice App)]
sharedSplices = [ ("currentUser", currentUserSplice)
                , ("isCurrentUserAdmin", isCurrentUserAdminSplice)
                ]

----------------------------------------------------------------------------

-- | Current @User@ splice. Diff with Snaplet-Auth.loggedInUser which return @AuthUser@
--
currentUserSplice :: SnapletISplice App
currentUserSplice = lift findCurrentUser
                    >>= I.textSplice . _userDisplayName

-- | Whether current user has admin role.
--
isCurrentUserAdminSplice :: SnapletISplice App
isCurrentUserAdminSplice = do
    tf <- lift isCurrentUserAdmin
    if tf then I.runChildren else return []
