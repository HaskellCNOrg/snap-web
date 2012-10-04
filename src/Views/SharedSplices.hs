{-# LANGUAGE CPP                  #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE OverloadedStrings    #-}

module Views.SharedSplices where

import           Data.Maybe            (fromMaybe)
import qualified Data.Text             as T
import           Snap.Snaplet.Heist
import           Text.Templating.Heist
import qualified Text.XmlHtml          as X

import           Application
import           Models.User


----------------------------------------------------------------------------

currentEnv :: T.Text
#ifdef DEVELOPMENT
currentEnv = "devel"
#else
currentEnv = "prod"
#endif

----------------------------------------------------------------------------


sharedSplices :: [(T.Text, SnapletSplice App App)]
sharedSplices = [ ("currentUser", currentUserSplice)
                , ("isCurrentUserAdmin", isCurrentUserAdminSplice)
                , ("showOnEnv", liftHeist showOnEnvSplice)
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

-- | Show children per Production or Development
--   e.g. <showOnEnv on="devel">.../</showOnEnv>
--
showOnEnvSplice :: Splice AppHandler
showOnEnvSplice = do
    node <- getParamNode
    if getOnAttr node == currentEnv then return $ X.elementChildren node else return []
    where getOnAttr n = fromMaybe "" (X.getAttribute "on" n)

