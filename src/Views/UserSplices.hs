{-# LANGUAGE OverloadedStrings, ExtendedDefaultRules #-}

module Views.UserSplices
       ( userDetailSplices ) where

import           Control.Arrow (second)
import           Control.Monad.Trans
import           Data.Maybe (fromJust)
import           Data.Maybe (isJust)
import           Snap.Snaplet.Auth
import           Text.Templating.Heist
import qualified Data.Text as T

import           Application
import Models.Exception
import Models.User
import Views.ExceptionSplices
import Views.Utils


------------------------------------------------------------------------------

-- | Splices used at user Detail page. 
--   Display either a user or error msg.    
-- 
userDetailSplices :: User -> [(T.Text, Splice AppHandler)]
userDetailSplices u = userDetailSplices' u


userDetailSplices' :: User -> [(T.Text, Splice AppHandler)]
userDetailSplices' u = [ ("ifUser", renderUser u) ]
                          --, ("ifuserError", renderUE e)]

------------------------------------------------------------------------------

-- | Single user to Splice
-- 
--renderuserSimple :: user -> Splice AppHandler
--renderuserSimple tag = runChildrenWithText 
--      [ ("userTitle", _title tag)
--      , ("userAuthor", _author tag)
--      , ("oid", userIdToText tag) ]

renderUser :: User -> Splice AppHandler
renderUser user = runChildrenWithText $
                     [ ("userLogin", userLogin $ _authUser user)
                     , ("userLastLoginAt", formatUTCTime' $ userLastLoginAt $ _authUser  user)
                     , ("userCreatedAt", formatUTCTime' $ userCreatedAt $ _authUser  user)
                     , ("userId", unUid $ fromJust $ userId $ _authUser user)
                     , ("userEmail", _userEmail user)
                     , ("userDisplayName", _displayName user) ]
                  where formatUTCTime' Nothing  = ""
                        formatUTCTime'  (Just x) = formatUTCTime x 

