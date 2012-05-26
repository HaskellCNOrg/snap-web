{-# LANGUAGE OverloadedStrings, ExtendedDefaultRules #-}

module Views.UserSplices where

import           Control.Arrow (second)
import           Control.Monad.Trans
import           Data.Maybe (fromJust, isJust)
import           Snap.Snaplet.Auth
import           Text.Templating.Heist
import qualified Data.Text as T

import           Application
import Models.Exception
import Models.User
import Views.ExceptionSplices
import Views.Utils
import Views.Types

------------------------------------------------------------------------------

instance SpliceRenderable User where
   toSplice = renderUser

------------------------------------------------------------------------------

-- | Splices used at user Detail page. 
--   Display either a user or error msg.    
-- 

userDetailSplices :: SpliceRenderable a => Either UserException a -> [(T.Text, Splice AppHandler)]
userDetailSplices ei = eitherToSplices ei


------------------------------------------------------------------------------

-- | Single user to Splice.
-- 
renderUser :: User -> Splice AppHandler
renderUser user = runChildrenWithText
                     [ ("userLogin",       userLogin $ _authUser user)
                     , ("userLastLoginAt", formatUTCTime' $ userLastLoginAt $ _authUser user)
                     , ("userCreatedAt",   formatUTCTime' $ userCreatedAt $ _authUser user)
                     , ("userEmail",       _userEmail user)
                     , ("userDisplayName", _displayName user) ]
                  where formatUTCTime' Nothing  = ""
                        formatUTCTime'  (Just x) = formatUTCTime x 

