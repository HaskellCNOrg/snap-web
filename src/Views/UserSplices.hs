{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE OverloadedStrings    #-}

module Views.UserSplices where

import           Control.Arrow       (second)
import           Control.Monad.Trans
import           Data.Maybe          (fromMaybe)
import qualified Data.Text           as T
import qualified Heist.Interpreted   as I
import           Snap.Snaplet.Auth

import           Application
import           Models.Exception
import           Models.User
import           Views.Types
import           Views.Utils

------------------------------------------------------------------------------

instance SpliceRenderable User where
   toSplice = renderUser

------------------------------------------------------------------------------

-- | Splices used at user Detail page.
--   Display either a user or error msg.
--

userDetailSplices :: Either UserException User -> [(T.Text, I.Splice AppHandler)]
userDetailSplices = eitherToSplices


------------------------------------------------------------------------------

-- | Single user to Splice.
--
renderUser :: User -> I.Splice AppHandler
renderUser user = I.runChildrenWith $
                     [ ("userEditable", hasEditPermissionSplice user)
                     --, ("userLastLoginAt", userLastLoginAtSplice $ _authUser user)
                     , ("isAuthUser", isAuthUserRetrieved $ _authUser user)
                     ]
                     ++
                     map (second I.textSplice)
                     [ ("userLogin",       maybe "" userLogin $ _authUser user)
                     , ("userEmail",       _userEmail user)
                     , ("userDisplayName", _userDisplayName user)
                     , ("userSite", fromMaybe "" $ _userSite user)
                     ]

formatUTCTimeMaybe Nothing  = ""
formatUTCTimeMaybe (Just x) = formatUTCTime x


isAuthUserRetrieved :: Maybe AuthUser
                       -> I.Splice AppHandler
isAuthUserRetrieved Nothing = return []
isAuthUserRetrieved (Just authusr) =
  I.runChildrenWithText
  [ ("lastLoginTime", formatUTCTimeMaybe $ userLastLoginAt authusr)
  , ("createdAt", formatUTCTimeMaybe $ userCreatedAt authusr)
  ]

----------------------------------------------------------------------------

-- | Has Edit premission when either current user is Admin or Author.
--
hasEditPermissionSplice :: User   -- ^ Author of some.
                        -> I.Splice AppHandler
hasEditPermissionSplice author = do
    has <- lift $ hasUpdatePermission author
    if has then I.runChildren else return []

----------------------------------------------------------------------------
