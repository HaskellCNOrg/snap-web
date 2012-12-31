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
{-
currentEnv :: T.Text
#ifdef DEVELOPMENT
currentEnv = "devel"
#else
currentEnv = "prod"
#endif
-}

-- | DEPRECATED a seprated layout-css.tpl is created and do replacement at prod build.
--
currentEnv :: AppHandler T.Text
currentEnv = lookupConfigDefault "env" "devel"

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
    v <- lift currentEnv
    return (if getOnAttr node == v then X.elementChildren node else [])
    where getOnAttr n = fromMaybe "" (X.getAttribute "on" n)
