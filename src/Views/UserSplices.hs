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
userDetailSplices :: AuthUser -> [(T.Text, Splice AppHandler)]
userDetailSplices u = userDetailSplices' u


userDetailSplices' :: AuthUser -> [(T.Text, Splice AppHandler)]
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

renderUser :: AuthUser -> Splice AppHandler
renderUser user = runChildrenWithText $
                     [ ("userLogin", userLogin user)
                     , ("userLastLoginAt", formatUTCTime' $ userLastLoginAt user)
                     , ("userCreatedAt", formatUTCTime' $ userCreatedAt user)
                     , ("userId", unUid $ fromJust $ userId user) ]
                  where formatUTCTime' Nothing  = ""
                        formatUTCTime'  (Just x) = formatUTCTime x 

