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
import qualified Data.ByteString       as BS
import Models.Utils (bsToText)

----------------------------------------------------------------------------

sharedSplices :: [(T.Text, SnapletISplice App)]
sharedSplices = [ ("currentUser", currentUserSplice)
                , ("isCurrentUserAdmin", isCurrentUserAdminSplice)
                , ("goto", currentURISplice)
                ]

----------------------------------------------------------------------------

nextPageParam :: BS.ByteString
nextPageParam = "goto"

genNextPageParam :: AppHandler BS.ByteString
genNextPageParam = liftM (gen . rqURI) getRequest
  where gen uri
          | uri == "/" = ""
          | otherwise = foldr1 BS.append ["?", nextPageParam, "=", uri]

-- | current URI
currentURISplice :: SnapletISplice App
currentURISplice = lift currentUri >>= I.textSplice
                   where currentUri = liftM bsToText genNextPageParam

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
