{-# LANGUAGE OverloadedStrings, ExtendedDefaultRules #-}

module Views.UserSplices where

import           Snap.Snaplet.Auth
import           Text.Templating.Heist
import qualified Data.Text as T

import           Application
import Models.Exception
import Models.User
import Views.Utils
import Views.Types

------------------------------------------------------------------------------

instance SpliceRenderable User where
   toSplice = renderUser

------------------------------------------------------------------------------

-- | Splices used at user Detail page. 
--   Display either a user or error msg.    
-- 

userDetailSplices :: Either UserException User -> [(T.Text, Splice AppHandler)]
userDetailSplices = eitherToSplices


------------------------------------------------------------------------------

-- | Single user to Splice.
-- 
renderUser :: User -> Splice AppHandler
renderUser user = runChildrenWithText
                     [ ("userLogin",       userLogin $ _authUser user)
                     , ("userLastLoginAt", formatUTCTime' $ userLastLoginAt $ _authUser user)
                     , ("userCreatedAt",   formatUTCTime' $ userCreatedAt $ _authUser user)
                     , ("userEmail",       _userEmail user)
                     , ("userDisplayName", _userDisplayName user)
                     , ("userSite", _userSite user) ]
                  where formatUTCTime' Nothing  = ""
                        formatUTCTime'  (Just x) = formatUTCTime x 


----------------------------------------------------------------------------
